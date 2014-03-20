(*********************************************************************************)
(*                Stog-rdf                                                       *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** RDF Plugin.
  Handle [<rdf*>] nodes to create and RDF graphs.
*)

open Stog_types;;

let dbg = Stog_misc.create_log_fun
  ~prefix: "Stog-rdf"
    "STOG_RDF_DEBUG_LEVEL"
;;
let module_name = "rdf";;

let rc_file stog = Stog_plug.plugin_config_file stog module_name;;

let keep_pcdata =
  let rec iter b = function
    [] -> Buffer.contents b
  | (Xtmpl.D s) :: q -> Buffer.add_string b s; iter b q
  | _ :: q -> iter b q
  in
  fun xmls -> iter (Buffer.create 256) xmls
;;

module IMap = Rdf_iri.Irimap;;

type rdf_data =
  {
    out_file : string ;
    graph_by_doc : Rdf_graph.graph Stog_path.Map.t;
    namespaces : (Rdf_iri.iri * string) list option ;
    loaded_graphs : Rdf_graph.graph IMap.t ;
    graph : (Rdf_graph.graph * Rdf_xml.global_state) option ;
    dataset : Rdf_ds.dataset option ;
    final_graph : Rdf_graph.graph option ;
  }

let empty_data =
  {
    out_file = "graph.rdf" ;
    graph_by_doc = Stog_path.Map.empty ;
    namespaces = None ;
    loaded_graphs = IMap.empty ;
    graph = None ;
    dataset = None ;
    final_graph = None ;
  }
;;

type source =
  | Src_file of string
  | Src_iri of Rdf_iri.iri
  | Src_str of string
;;

let string_of_source = function
  Src_file f -> "file " ^ f
| Src_iri i -> "iri " ^ (Rdf_iri.string i)
| Src_str _ -> "<inline code>"
;;

let rdf_iri s =
  try Rdf_iri.iri s
  with
  | Rdf_iri.Invalid_iri (s, msg) ->
      failwith ("Invalid IRI "^s^" ("^msg^")")
  | e ->
      let msg = Printf.sprintf "While making uri from %S: %s" s
        (Printexc.to_string e)
      in
      failwith msg
;;

let namespaces data =
  match data.namespaces with
    Some ns -> ns
  | None -> assert false
;;

let add_loaded_graph data name g =
  { data with loaded_graphs = IMap.add name g data.loaded_graphs }
;;

let get_loaded_graph data name =
  try IMap.find name data.loaded_graphs
  with _ -> failwith ("Graph "^(Rdf_iri.string name)^" not loaded.")
;;

let load_graph acc_data ?doc name ?data options =
  Stog_plug.verbose ("Load graph "^(Rdf_iri.string name)^"...");
  Stog_plug.verbose ~level:2
    ("Options:"^
     (String.concat ", "
      (List.map(fun (n,v) -> n^"="^v) options)));

  let g =
    try get_loaded_graph acc_data name
    with _ ->
        let file =
          try Some (List.assoc "file" options)
          with Not_found -> None
        in
        match file with
          None ->
            begin
              match data with
                None -> Rdf_graph.open_graph ~options name
              | Some s ->
                  try
                    let g = Rdf_graph.open_graph name in
                    let ttl =
                      List.fold_right
                        (fun (iri, s) acc ->
                           "@prefix "^s^": <"^(Rdf_iri.string iri)^"> .\n"^acc)
                        (namespaces acc_data) s
                    in
                    Rdf_ttl.from_string g ttl;
                    g
                  with
                    Rdf_ttl.Error e ->
                      failwith (Rdf_ttl.string_of_error e)
            end
        | Some file ->
            try
              let file =
                match doc with
                  None -> file
                | Some doc ->
                    if Filename.is_relative file && Filename.is_implicit file then
                      Filename.concat (Filename.dirname doc.Stog_types.doc_src) file
                    else
                      file
              in
              let g = Rdf_graph.open_graph name in
              let from =
                match
                  try String.lowercase (List.assoc "format" options)
                  with Not_found -> ""
                with
                  "rdf/xml" -> Rdf_xml.from_file
                | _ -> Rdf_ttl.from_file
              in
              from g file ;
              g
            with
            | Rdf_ttl.Error e ->
                failwith (Rdf_ttl.string_of_error e)
            | Rdf_xml.Invalid_rdf s ->
                failwith ("Invalid RDF: "^s)
  in
  let acc_data = add_loaded_graph acc_data name g in
  Stog_plug.verbose "done";
  acc_data
;;

let rule_load_graph rule_tag doc_id (stog, acc_data) env args xmls =
  let doc = Stog_types.doc stog doc_id in
  let name =
     match Xtmpl.get_arg_cdata args ("", "name") with
       None -> failwith (rule_tag^": missing name attribute")
     | Some s -> rdf_iri s
  in
  let options =
    Xtmpl.Name_map.fold
      (fun op v acc ->
         match op with
           ("","name") -> acc
         | ("",s) -> (s, keep_pcdata v) :: acc
         | _ -> acc)
      args []
  in
  let data = match xmls with [] -> None | _ -> Some (keep_pcdata xmls) in
  let acc_data = load_graph acc_data ~doc name ?data options in
  ((stog, acc_data), [])
;;

let read_options (stog,data) =
  let module CF = Config_file in
  let group = new CF.group in
  let ns = new CF.list_cp
    (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers) ~group
    ["namespaces"] [] "pairs (uri, name) specifying namespaces"
  in
  let graph_file = new CF.string_cp ~group ["graph_file"] data.out_file
    "name of main graph output file"
  in
  let sources = new CF.list_cp
    (CF.tuple2_wrappers CF.string_wrappers
     (CF.list_wrappers (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers)))
      ~group
      ["sources"] []
      "pairs (uri, options) specifying graphs to load and associate to uris. Options is a list of pair (name, value). To load a load, use [\"file\", \"myfile.ttl\"]. Other options can be given to access graphs from a database (see OCaml-RDF's Rdf_graph documentation)"
  in
  let rc_file = rc_file stog in
  group#read rc_file;
  group#write rc_file;

  let data = { data with out_file = graph_file#get } in

  let ns = List.fold_left
    (fun acc (iri, name) -> (rdf_iri iri, name) :: acc)
    [ Rdf_iri.of_uri (Rdf_uri.of_neturl stog.Stog_types.stog_base_url), "site" ;
      Rdf_rdf.rdf_ "", "rdf" ;
    ]
    ns#get
  in
  let data = { data with namespaces = Some ns } in
  let data = List.fold_left
    (fun data (iri, options) -> load_graph data (rdf_iri iri) options)
      data sources#get
  in
  (stog, data)
;;

let init env (stog,data) _ = read_options (stog,data);;
let fun_level_init = Stog_engine.Fun_stog_data init;;

let build_ns_map namespaces =
  let pred iri name iri2 name2 =
    name = name2 && not (Rdf_iri.equal iri iri2)
  in
  let f map (iri, name) =
    try
      let name2 = IMap.find iri map in
      let msg = Printf.sprintf "%S is already associated to name %S"
        (Rdf_iri.string iri) name2
      in
      failwith msg
    with
      Not_found ->
        if IMap.exists (pred iri name) map then
          (
           let msg = Printf.sprintf
             "Namespace %S already associated to an IRI different from %S"
             name (Rdf_iri.string iri)
           in
           failwith msg
          )
        else
          IMap.add iri name map
  in
  List.fold_left f IMap.empty namespaces
;;

let apply_namespaces =
  let map_qn ns = function
    ("",s) ->
      dbg ~level: 2 (fun () -> "map_qn: no prefix: "^s);
      ("",s)
  | (pref, s) ->
      dbg ~level: 2 (fun () -> "map_qn pref="^pref^", s="^s);
      try
        let p = Rdf_xml.SMap.find pref ns in
        (Rdf_iri.string p, s)
      with Not_found ->
          dbg ~level: 2 (fun () -> "not found");
          (pref, s)
  in
  let rec iter ns xml =
    match xml with
      Rdf_xml.D _ -> xml
    | Rdf_xml.E ((tag,atts),subs) ->
       let tag = map_qn ns tag in
       let atts = List.map (fun (qn,v) -> (map_qn ns qn, v)) atts in
       let subs = List.map (iter ns) subs in
       Rdf_xml.E ((tag, atts), subs)
  in
  fun ns xml ->
    let ns = IMap.fold
      (fun iri pref map -> Rdf_xml.SMap.add pref iri map)
       ns Rdf_xml.SMap.empty
    in
    iter ns xml
;;

let set_graph data x = { data with graph = Some x } ;;
let graph (stog, data) =
  match data.graph with
    Some x -> ((stog,data), x)
  | None ->
      let g = Rdf_graph.open_graph
        (Rdf_iri.of_uri (Rdf_uri.of_neturl stog.Stog_types.stog_base_url))
      in
      let namespaces = namespaces data in
      let gstate = {
          Rdf_xml.blanks = Rdf_xml.SMap.empty ;
          gnamespaces = build_ns_map namespaces ;
        }
      in
      let data = set_graph data (g, gstate) in
      ((stog, data), (g, gstate))
;;

let set_dataset data x = { data with dataset = Some x };;
let dataset (stog, data) =
  match data.dataset with
    Some d -> ((stog, data), d)
  | None ->
      let g =
        match data.final_graph with
          None -> failwith "No final graph!"
        | Some g -> g
      in
      let loaded =
        IMap.fold
          (fun _ g acc -> (g.Rdf_graph.name (), g) :: acc)
          data.loaded_graphs []
      in
      let named =
        Stog_path.Map.fold
          (fun _ g acc -> (g.Rdf_graph.name (), g) :: acc)
          data.graph_by_doc loaded
      in
      let d = Rdf_ds.simple_dataset ~named g in
      let data = set_dataset data d in
      ((stog, data), d)
;;

let tag_of_string s =
  try
    let len = String.length s in
    let p = String.index s ':' in
    if len > p then
      (String.sub s 0 p, String.sub s (p+1) (len - p - 1))
    else
      ("", String.sub s 0 p)
  with Not_found -> ("",s)


let get_rdf_resource (stog,data) env atts =
  match Xtmpl.get_arg_cdata atts ("","obj") with
    (Some _) as x -> ((stog, data), x)
  | None ->
      try
        let href =
          match Xtmpl.get_arg_cdata atts ("", "href") with
            None -> raise Not_found
          | Some s -> s
        in
        dbg ~level: 3 (fun () -> "get_rdf_resource href="^href);
        let ((stog, data), e) = Stog_plug.doc_by_href stog (stog, data) env href in
        match e with
          None -> raise Not_found
        | Some (doc, path, idopt) ->
            let url =
              let url = Stog_engine.doc_url stog doc in
              match idopt with
                None -> url
              | Some s -> Neturl.modify_url ~fragment: s url
            in
            ((stog, data), Some (Stog_types.string_of_url url))
      with
        Not_found ->
          dbg ~level: 3 (fun () -> "href not found");
          ((stog, data), None)
;;

let merge_cdata =
  let rec f acc = function
    [] -> List.rev acc
  | (Xtmpl.D s1) :: (Xtmpl.D s2) :: q -> f acc ((Xtmpl.D (s1^s2)) :: q)
  | ((Xtmpl.D _) as x) :: q -> f (x :: acc) q
  | (Xtmpl.E (t, atts, subs)) :: q ->
      let subs = f [] subs in
      f ((Xtmpl.E (t, atts, subs)) :: acc) q
  in
  f []
;;

let rec map_to_rdf_xml_tree = function
  Xtmpl.D s -> Rdf_xml.D s
| Xtmpl.E (t, atts, subs) ->
    let subs = List.map map_to_rdf_xml_tree subs in
    let atts = Xtmpl.string_of_xml_atts atts in
    Rdf_xml.E ((t, atts), subs)
;;

let rec map_to_xml_tree = function
  Rdf_xml.D s -> Xtmpl.D s
| Rdf_xml.E ((t, atts), subs) ->
    let subs = List.map map_to_xml_tree subs in
    let atts = Xtmpl.xmls_of_atts atts in
    Xtmpl.E (t, atts, subs)
;;

let prerr_atts l =
  prerr_endline "attributes:";
  List.iter
  (fun ((s1,s2),v) -> prerr_endline (Printf.sprintf "((%S, %S), %S)" s1 s2 v))
  l
;;

let parse_prop (stog,data) env g subject atts gstate subs =
  let subject =
    match Xtmpl.get_arg_cdata atts ("","subject") with
        None -> subject
      | Some iri -> Rdf_iri.iri iri
  in
  let pred =
    match Xtmpl.get_arg_cdata atts ("","pred") with
      None -> failwith "Missing \"pred\" attribute for rdf node"
    | Some s -> s
  in
  let tag = tag_of_string pred in
  dbg ~level: 3 (fun () -> Printf.sprintf "parse_prop subject=%s, pred=%s"
     (Rdf_iri.string subject) pred);
  let ((stog,data), rdf_resource) = get_rdf_resource (stog,data) env atts in
  let atts = List.fold_left
    (fun atts name -> Xtmpl.Name_map.remove ("", name) atts)
      atts ["subject" ; "pred" ; "href" ; "obj" ;
        Xtmpl.att_defer ; Xtmpl.att_protect ; Xtmpl.att_escamp ]
  in
  let atts =
    match rdf_resource with
      None -> atts
    | Some uri ->
        Xtmpl.atts_one ~atts ("", Rdf_iri.string Rdf_rdf.rdf_resource) [Xtmpl.D uri]
  in
  let state = {
      Rdf_xml.subject = Some (Rdf_term.Iri subject) ;
      predicate = None ;
      xml_base = Rdf_iri.of_uri (Rdf_uri.of_neturl stog.Stog_types.stog_base_url) ;
      xml_lang = None ;
      datatype = None ;
      namespaces = gstate.Rdf_xml.gnamespaces ; (*IMap.empty ;*)
    }
  in
  let subs = List.map map_to_rdf_xml_tree (merge_cdata subs) in
  let atts = Xtmpl.string_of_xml_atts atts in
  let node = Rdf_xml.E ((tag, atts), subs) in
  let node = apply_namespaces gstate.Rdf_xml.gnamespaces node in
  dbg ~level: 2 (fun () -> (Printf.sprintf "rdf node:\n%s"
      (Xtmpl.string_of_xml (map_to_xml_tree node))));

  let (gstate, _) = Rdf_xml.input_prop g state (gstate, 0) node in
  ((stog, data), gstate)
;;

let gather =
  let rec iter env doc_url subj_id g ((stog,data), gstate) = function
    Xtmpl.D _ -> ((stog, data), gstate)
  | Xtmpl.E (("","rdf"), atts, subs) ->
      let subject =
        let uri =
          match subj_id with
            None -> doc_url
          | Some id -> Neturl.modify_url ~fragment: id doc_url
        in
        Rdf_iri.of_uri (Rdf_uri.of_neturl uri)
      in
      parse_prop (stog,data) env g subject atts gstate subs
  | Xtmpl.E (_, atts, subs) ->
      let subj_id =
        match Xtmpl.get_arg_cdata atts ("", "id") with
          None -> subj_id
        | Some s -> Some s
      in
      iter_list env doc_url subj_id g ((stog,data), gstate) subs
  and iter_list env doc_url subj_id g ((stog,data), gstate) l =
    List.fold_left (iter env doc_url subj_id g) ((stog,data),gstate) l
  in
  fun (stog,data) env g doc gstate xmls ->
    let doc_url = Stog_engine.doc_url stog doc in
    iter_list env doc_url None g ((stog, data), gstate) xmls
;;

let create_graph ?doc (stog,data) =
  let base_url =
    match doc with
      None -> stog.Stog_types.stog_base_url
    | Some doc -> Stog_engine.doc_url stog doc
  in
  let g = Rdf_graph.open_graph (Rdf_iri.of_uri (Rdf_uri.of_neturl base_url)) in
  let namespaces = namespaces data in
  let gstate = {
      Rdf_xml.blanks = Rdf_xml.SMap.empty ;
      gnamespaces = build_ns_map namespaces ;
    }
  in
  (g, gstate)
;;

let add_doc_graph data doc g =
  { data with
    graph_by_doc = Stog_path.Map.add
      doc.doc_path g data.graph_by_doc ;
  }
;;

let make_graph =
  let make env doc_id (stog, data) =
    let doc = Stog_types.doc stog doc_id in
    let (g, gstate) = create_graph ~doc (stog,data) in
    let xmls =
      match doc.Stog_types.doc_out with
        None -> doc.Stog_types.doc_body
      | Some x -> x
    in
    try
      let env =
        Xtmpl.env_of_list ~env
          [("", Stog_tags.doc_path),
            (fun  acc _ _ _ -> (acc, [Xtmpl.D (Stog_path.to_string doc.doc_path)]))
          ]
      in
      let ((stog, data), _) = gather (stog, data) env g doc gstate xmls in
      let data = add_doc_graph data doc g in
      let stog = Stog_types.set_doc stog doc_id doc in
      (stog, data)
    with
      e ->
        (*
           let s = Xtmpl.string_of_xmls xmls in
           prerr_endline s;
           *)
        raise e
  in
  fun env acc doc_ids -> Stog_types.Doc_set.fold (make env) doc_ids acc 
;;

let output_graph _ (stog,data) _ =
  let (g, _) = create_graph (stog,data) in
  let namespaces = namespaces data in
  Stog_path.Map.iter
    (fun _ g_doc -> Rdf_graph.merge g g_doc) data.graph_by_doc;
  let out_file = Filename.concat stog.Stog_types.stog_outdir data.out_file in
  Rdf_xml.to_file ~namespaces g out_file;
  Stog_plug.verbose (Printf.sprintf "RDF graph dumped into %S" out_file);
  let dot = Rdf_dot.dot_of_graph ~namespaces g in
  Stog_misc.file_of_string ~file: ((Filename.chop_extension out_file)^".dot") dot;
  let data = { data with final_graph = Some g } in
  (stog, data)
;;

let rdf_string_of_term = Rdf_term.string_of_term ;;

let string_of_term t =
  match Rdf_dt.of_term t with
  | Rdf_dt.Err e -> Rdf_dt.string_of_error e
  | Rdf_dt.Blank s -> "_:"^s
  | Rdf_dt.Iri iri -> Rdf_iri.string iri
  | Rdf_dt.String s -> s
  | Rdf_dt.Int n -> string_of_int n
  |	Rdf_dt.Float f -> string_of_float f
  | Rdf_dt.Bool true -> "true"
  | Rdf_dt.Bool false -> "false"
  |	Rdf_dt.Datetime netdate ->
      Netdate.format ~fmt: Rdf_dt.date_fmt netdate
  | Rdf_dt.Ltrl (s,_) -> s
  | Rdf_dt.Ltrdt (s, _) -> s
;;

type query_spec =
  { query : string ;
    tmpl : Xtmpl.tree list ;
    separator : Xtmpl.tree list ;
    args : Xtmpl.attributes ;
  }

let apply_select_sol env stog doc tmpl sol =
  let esc f s = [ Xtmpl.D (f s) ] in
  let atts =
    Rdf_sparql.solution_fold
      (fun name term acc ->
         Xtmpl.atts_of_list ~atts: acc
           [ ("", name), esc string_of_term term ;
             ("", name^"_rdf"), esc rdf_string_of_term term ;
           ]
      )
      sol
      Xtmpl.atts_empty
  in
  [Xtmpl.E (("",Xtmpl.tag_env), atts, tmpl)]
;;

let apply_select_sols env stog doc query sols =
  let rec iter acc = function
    [] -> List.rev acc
  | sol :: q ->
     let xml = apply_select_sol env stog doc query.tmpl sol in
     match q with
       [] -> iter (xml @ acc) q
     | _ -> iter (query.separator @ xml @ acc) q
  in
  let xmls = iter [] sols in
  match Xtmpl.Name_map.is_empty query.args with
    true -> xmls
  | _ -> [Xtmpl.E (("", Xtmpl.tag_env), query.args, xmls)]
;;

let read_select_query_from_atts stog doc query args =
  let f name v (stog, query) =
    match name with
    | ("", "sep") ->
        (stog, { query with separator = v })
    | ("", "query") ->
        (stog, { query with query = Xtmpl.string_of_xmls v })
    | ("", "tmpl") ->
        let (stog, tmpl) =
          Stog_tmpl.read_template_file stog doc (Xtmpl.string_of_xmls v)
        in
        (stog, { query with tmpl = [tmpl] })
    | (prefix, att) ->
        (stog,
         { query with
           args = Xtmpl.atts_one ~atts: query.args (prefix, att) v ;
         })
  in
  Xtmpl.Name_map.fold f args (stog, query)
;;

let rec read_select_query_from_xmls stog doc query = function
  [] -> (stog, query)
| (Xtmpl.D _) :: q -> read_select_query_from_xmls stog doc query q
| (Xtmpl.E (tag,_,xmls)) :: q ->
    (*prerr_endline ("tag=("^(fst tag)^","^(snd tag)^")");*)
    let query =
      match tag with
        ("", "tmpl") -> { query with tmpl = xmls }
      | ("", "sep") -> { query with separator = xmls}
      | ("", "query") -> { query with query = keep_pcdata xmls }
      | (prefix, tag) ->
          { query with
            args = Xtmpl.atts_one ~atts: query.args (prefix, tag) xmls ;
          }
    in
    read_select_query_from_xmls stog doc query q
;;

let build_select_query stog doc env args subs =
  let q = { query = "" ; tmpl = [] ; separator = [] ; args = Xtmpl.atts_empty } in
  let (stog, q) = read_select_query_from_atts stog doc q args in
  let with_xmls = List.exists
    (function Xtmpl.E _ -> true | _ -> false)
    subs
  in
  match with_xmls, subs with
    true, _ -> read_select_query_from_xmls stog doc q subs
  | false, [] -> (stog, q)
  | false, _ -> (stog, { q with query = keep_pcdata subs })
;;

let exec_select (stog,data) doc env query =
  try
    let ((stog,data), dataset) = dataset (stog, data) in
    (*Rdf_ttl.to_file dataset.Rdf_ds.default "/tmp/rdfselect.ttl";*)
    let q = Rdf_sparql.query_from_string query.query in
    (*prerr_endline ("query: "^query.query);*)
    let res = Rdf_sparql.execute
      ~base: (Rdf_iri.of_uri (Rdf_uri.of_neturl stog.Stog_types.stog_base_url))
      dataset q
    in
    match res with
      Rdf_sparql.Solutions sols ->
        Stog_msg.verbose ~level: 2
          (Printf.sprintf "%d solutions for query %s" (List.length sols) query.query);
        ((stog,data), apply_select_sols env stog doc query sols)
    | _ ->
        failwith "rdf-select did not return solutions"
  with
    Rdf_sparql.Error e ->
      let msg = Rdf_sparql.string_of_error e in
      failwith ("SPARQL:\n"^query.query^"\n"^msg)
  | e ->
      let msg = Printexc.to_string e in
      failwith ("rdf-select:\n"^query.query^"\n"^msg)
;;

let fun_rdf_select doc_id (stog,data) env args subs =
  let doc = Stog_types.doc stog doc_id in
  let (stog, query) = build_select_query stog doc env args subs in
  if query.tmpl = [] then
      failwith "Missing or empty template for rdf-select";
  let query =
    match query.query with
      "" -> failwith "No query for rdf-select";
    | s ->
        (* add default namespaces as header *)
        let s = List.fold_right
          (fun (iri, s) acc ->
             "PREFIX "^s^": <"^(Rdf_iri.string iri)^">\n"^acc)
            (namespaces data) s
        in
        { query with query = s }
  in
  exec_select (stog,data) doc env query
;;

let rules_rdf_select stog doc_id =
  [("", "rdf-select"), fun_rdf_select doc_id]
;;

let rules_rdf_load stog doc_id =
  [ ("", "rdf"), (fun acc _ _ _ -> (acc, [])) ;
    ("", "rdf-load"), (rule_load_graph "rdf-load" doc_id) ;
  ];;


let level_funs =
  [
    "init", fun_level_init ;
    "make-graph", Stog_engine.Fun_stog_data make_graph ;
    "output-graph", Stog_engine.Fun_stog_data output_graph ;
    "load", Stog_engine.fun_apply_stog_data_doc_rules rules_rdf_load ;
    "select", Stog_engine.fun_apply_stog_data_doc_rules rules_rdf_select ;
  ]
;;

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [
      "init", [ -1 ] ;
      "make-graph", [ 200 ] ;
      "output-graph", [ 201 ] ;
      "load", [ 202 ] ;
      "select", [ 220 ] ;
    ]

let make_engine ?levels () =
  let levels = Stog_html.mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = rdf_data
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = empty_data ;
       }

    type cache_data = string

    let cache_load stog data doc xml =
      let (g,_) = create_graph ~doc (stog, { data with namespaces = Some [] }) in
      let base = Rdf_iri.of_uri (Rdf_uri.of_neturl (Stog_engine.doc_url stog doc)) in
      Rdf_xml.from_string g ~base xml ;
      add_doc_graph data doc g

    let cache_store stog data doc =
      let g =
        try Stog_path.Map.find doc.doc_path data.graph_by_doc
        with Not_found -> fst (create_graph ~doc (stog,data))
      in
      let namespaces = namespaces data in
      Rdf_xml.to_string ~namespaces g
  end
  in
  (module M : Stog_engine.Module)
;;

let f stog =
  let levels =
    try Some (Stog_types.Str_map.find module_name stog.Stog_types.stog_levels)
    with Not_found -> None
  in
  make_engine ?levels ()
;;

let () = Stog_engine.register_module module_name f;

