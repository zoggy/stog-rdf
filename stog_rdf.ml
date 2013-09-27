(*********************************************************************************)
(*                Stog-rdf                                                       *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              *)
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
  ~prefix: "Rdf_sparql_eval"
    "STOG_RDF_DEBUG_LEVEL"
;;
let plugin_name = "rdf";;

let rc_file stog = Stog_plug.plugin_config_file stog plugin_name;;


let keep_pcdata =
  let rec iter b = function
    [] -> Buffer.contents b
  | (Xtmpl.D s) :: q -> Buffer.add_string b s; iter b q
  | _ :: q -> iter b q
  in
  fun xmls -> iter (Buffer.create 256) xmls
;;

let graph_by_elt = ref Str_map.empty;;

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

let out_file = ref "graph.rdf";;

let namespaces_ = ref None;;
let namespaces () =
  match !namespaces_ with
    Some ns -> ns
  | None -> assert false
;;

module IMap = Rdf_iri.Irimap;;

let loaded_graphs = ref IMap.empty;;

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

let add_loaded_graph name g = loaded_graphs := IMap.add name g !loaded_graphs;;

let get_loaded_graph name =
  try IMap.find name !loaded_graphs
  with _ -> failwith ("Graph "^(Rdf_iri.string name)^" not loaded.")
;;


let load_graph ?elt name ?data options =
  Stog_plug.verbose ("Load graph "^(Rdf_iri.string name)^"...");
  Stog_plug.verbose ~level:2
    ("Options:"^
     (String.concat ", "
      (List.map(fun (n,v) -> n^"="^v) options)));

  let g =
    try get_loaded_graph name
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
                        (namespaces ()) s
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
                match elt with
                  None -> file
                | Some elt ->
                    if Filename.is_relative file && Filename.is_implicit file then
                      Filename.concat (Filename.dirname elt.Stog_types.elt_src) file
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
  add_loaded_graph name g;
  Stog_plug.verbose "done"
;;

let rule_load_graph rule_tag stog elt env args xmls =
  let name =
     match Xtmpl.get_arg args ("", "name") with
       None -> failwith (rule_tag^": missing name attribute")
     | Some s -> rdf_iri s
  in
  let options =
    List.fold_left
      (fun acc (op,v) ->
         match op with
           ("","name") -> acc
         | ("",s) -> (s, v) :: acc
         | _ -> acc)
      [] args
  in
  let data = match xmls with [] -> None | _ -> Some (keep_pcdata xmls) in
  load_graph ~elt name ?data options;
  []
;;

let read_options stog =
  let module CF = Config_file in
  let group = new CF.group in
  let ns = new CF.list_cp
    (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers) ~group
    ["namespaces"] [] "pairs (uri, name) specifying namespaces"
  in
  let graph_file = new CF.string_cp ~group ["graph_file"] !out_file
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

  out_file := graph_file#get;

  let ns = List.fold_left
    (fun acc (iri, name) -> (rdf_iri iri, name) :: acc)
    [ Rdf_iri.of_uri (Rdf_uri.of_neturl stog.Stog_types.stog_base_url), "site" ;
      Rdf_rdf.rdf_ "", "rdf" ;
    ]
    ns#get
  in
  namespaces_ := Some ns ;

  List.iter
    (fun (iri, options) -> load_graph (rdf_iri iri) options)
    sources#get
;;

let init stog = read_options stog; stog ;;
let () = Stog_plug.register_stage0_fun init;;

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

let graph = ref None;;
let set_graph x = graph := Some x;;
let graph () =
  match !graph with
    Some x -> x
  | None ->
      let stog = Stog_plug.stog () in
      let g = Rdf_graph.open_graph (Rdf_iri.of_uri (Rdf_uri.of_neturl stog.Stog_types.stog_base_url)) in
      let namespaces = namespaces () in
      let gstate = {
          Rdf_xml.blanks = Rdf_xml.SMap.empty ;
          gnamespaces = build_ns_map namespaces ;
        }
      in
      set_graph (g, gstate);
      (g, gstate)
;;
let final_graph = ref None;;

let dataset = ref None;;
let set_dataset x = dataset := Some x;;
let dataset stog =
  match !dataset with
    Some d -> d
  | None ->
      let g =
        match !final_graph with
          None -> failwith "No final graph!"
        | Some g -> g
      in
      let loaded =
        IMap.fold
          (fun _ g acc -> (g.Rdf_graph.name (), g) :: acc)
          !loaded_graphs []
      in
      let named =
        Str_map.fold
          (fun _ g acc -> (g.Rdf_graph.name (), g) :: acc)
          !graph_by_elt loaded
      in
      let d = Rdf_ds.simple_dataset ~named g in
      set_dataset d;
      d
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


let get_rdf_resource stog env atts =
  try
    Some (List.assoc ("","obj") atts)
  with
    Not_found ->
      try
        let href = List.assoc ("", "href") atts in
        match Stog_plug.elt_by_href stog env href with
          None -> raise Not_found
        | Some (elt, hid, idopt) ->
            let url =
              let url = Stog_html.elt_url stog elt in
              match idopt with
                None -> url
              | Some s -> Neturl.modify_url ~fragment: s url
            in
            Some (Stog_types.string_of_url url)
      with
        Not_found ->
          None
;;

let rec map_to_rdf_xml_tree = function
  Xtmpl.D s -> Rdf_xml.D s
| Xtmpl.E (t, atts, subs) ->
    let subs = List.map map_to_rdf_xml_tree subs in
    Rdf_xml.E ((t, atts), subs)
;;

let rec map_to_xml_tree = function
  Rdf_xml.D s -> Xtmpl.D s
| Rdf_xml.E ((t, atts), subs) ->
    let subs = List.map map_to_xml_tree subs in
    Xtmpl.E (t, atts, subs)
;;

let prerr_atts l =
  prerr_endline "attributes:";
  List.iter
  (fun ((s1,s2),v) -> prerr_endline (Printf.sprintf "((%S, %S), %S)" s1 s2 v))
  l
;;

let parse_prop stog env g subject atts gstate subs =
  let subject =
    try Rdf_iri.iri (List.assoc ("","subject") atts)
    with Not_found -> subject
  in
  let pred =
    try List.assoc ("","pred") atts
    with Not_found ->
        failwith "Missing \"pred\" attribute for rdf node"
  in
  let tag = tag_of_string pred in
  let rdf_resource = get_rdf_resource stog env atts in
  let atts = List.filter
    (function
         (("", "subject"), _) | (("","pred"),_) | (("","href"),_) | (("","obj"),_) -> false
     | _ -> true
    ) atts
  in
  let atts =
    match rdf_resource with
      None -> atts
    | Some uri ->
        (("", Rdf_iri.string Rdf_rdf.rdf_resource), uri) :: atts
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
  let subs = List.map map_to_rdf_xml_tree subs in
  let node = Rdf_xml.E ((tag, atts), subs) in
  let node = apply_namespaces gstate.Rdf_xml.gnamespaces node in
  dbg ~level: 2 (fun () -> (Printf.sprintf "rdf node:\n%s"
      (Xtmpl.string_of_xml (map_to_xml_tree node))));

  let (gstate, _) = Rdf_xml.input_prop g state (gstate, 0) node in
  gstate
;;

let gather =
  let rec iter stog env elt_url subj_id g gstate = function
    Xtmpl.D _ -> gstate
  | Xtmpl.E (("","rdf"), atts, subs) ->
      let subject =
        let uri =
          match subj_id with
            None -> elt_url
          | Some id -> Neturl.modify_url ~fragment: id elt_url
        in
        Rdf_iri.of_uri (Rdf_uri.of_neturl uri)
      in
      parse_prop stog env g subject atts gstate subs
  | Xtmpl.E (_, atts, subs) ->
      let subj_id =
         try Some (List.assoc ("", "id") atts)
         with Not_found -> subj_id
      in
      iter_list stog env elt_url subj_id g gstate subs
  and iter_list stog env elt_url subj_id g gstate l =
    List.fold_left (iter stog env elt_url subj_id g) gstate l
  in
  fun stog env g elt gstate xmls ->
    let elt_url = Stog_html.elt_url stog elt in
    iter_list stog env elt_url None g gstate xmls
;;

let create_graph ?elt stog =
  let base_url =
    match elt with
      None -> stog.Stog_types.stog_base_url
    | Some elt -> Stog_html.elt_url stog elt
  in
  let g = Rdf_graph.open_graph (Rdf_iri.of_uri (Rdf_uri.of_neturl base_url)) in
  let namespaces = namespaces () in
  let gstate = {
      Rdf_xml.blanks = Rdf_xml.SMap.empty ;
      gnamespaces = build_ns_map namespaces ;
    }
  in
  (g, gstate)
;;

let add_elt_graph elt g =
  graph_by_elt := Str_map.add
    (Stog_types.string_of_human_id elt.elt_human_id) g
    !graph_by_elt
;;

let make_graph env stog elt_id elt =
  let (g, gstate) = create_graph ~elt stog in
  let xmls =
    match elt.Stog_types.elt_out with
      None -> elt.Stog_types.elt_body
    | Some x -> x
  in
  try
    let env =
      Xtmpl.env_of_list ~env
        [("", Stog_tags.elt_hid),
          (fun  _ _ _ -> [Xtmpl.D (Stog_types.string_of_human_id elt.elt_human_id)])
        ]
    in
    ignore(gather stog env g elt gstate xmls);
    add_elt_graph elt g;
    elt
  with
    e ->
  (*
      let s = Xtmpl.string_of_xmls xmls in
      prerr_endline s;
  *)
      raise e
;;

let output_graph _ stog _ =
  let (g, _) = create_graph stog in
  let namespaces = namespaces () in
  Str_map.iter (fun _ g_elt -> Rdf_graph.merge g g_elt) !graph_by_elt;
  let out_file = Filename.concat stog.Stog_types.stog_outdir !out_file in
  Rdf_xml.to_file ~namespaces g out_file;
  Stog_plug.verbose (Printf.sprintf "RDF graph dumped into %S" out_file);
  let dot = Rdf_dot.dot_of_graph ~namespaces g in
  Stog_misc.file_of_string ~file: ((Filename.chop_extension out_file)^".dot") dot;
  final_graph := Some g;
  []
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
    args : Xtmpl.attribute list ;
  }

let apply_select_sol env stog elt tmpl sol =
  let esc f s = Xtmpl.string_of_xml (Xtmpl.D (f s)) in
  let atts =
    Rdf_sparql.solution_fold
      (fun name term acc ->
         (("", name), esc string_of_term term) ::
         (("", name^"_rdf"), esc rdf_string_of_term term) ::
         acc
      )
      sol
      []
  in
  [Xtmpl.E (("",Xtmpl.tag_env), atts, tmpl)]
;;

let apply_select_sols env stog elt query sols =
  let rec iter acc = function
    [] -> List.rev acc
  | sol :: q ->
     let xml = apply_select_sol env stog elt query.tmpl sol in
     match q with
       [] -> iter (xml @ acc) q
     | _ -> iter (query.separator @ xml @ acc) q
  in
  let xmls = iter [] sols in
  match query.args with
    [] -> xmls
  | _ -> [Xtmpl.E (("", Xtmpl.tag_env), query.args, xmls)]
;;

let rec read_select_query_from_atts stog elt query = function
  [] -> query
| arg :: q ->
    let query =
      match arg with
      | (("", "sep"), s) ->
          { query with separator = [Xtmpl.D s]}
      | (("", "query"), s) ->
          { query with query = s }
      | (("", "tmpl"), file) ->
          let tmpl = Stog_tmpl.read_template_file stog elt file in
          { query with tmpl = [tmpl] }
      | ((prefix, att), s) ->
          { query with
            args = ((prefix, att), s) :: query.args ;
          }
    in
    read_select_query_from_atts stog elt query q
;;

let rec read_select_query_from_xmls stog elt query = function
  [] -> query
| (Xtmpl.D _) :: q -> read_select_query_from_xmls stog elt query q
| (Xtmpl.E (tag,_,xmls)) :: q ->
    (*prerr_endline ("tag=("^(fst tag)^","^(snd tag)^")");*)
    let query =
      match tag with
        ("", "tmpl") -> { query with tmpl = xmls }
      | ("", "sep") -> { query with separator = xmls}
      | ("", "query") -> { query with query = keep_pcdata xmls }
      | (prefix, tag) ->
          { query with
            args = ((prefix, tag), Xtmpl.string_of_xmls xmls) :: query.args ;
          }
    in
    read_select_query_from_xmls stog elt query q
;;

let build_select_query stog elt env args subs =
  let q = { query = "" ; tmpl = [] ; separator = [] ; args = [] } in
  let q = read_select_query_from_atts stog elt q args in
  let q = { q with args = List.rev q.args } in
  let with_xmls = List.exists
    (function Xtmpl.E _ -> true | _ -> false)
    subs
  in
  match with_xmls, subs with
    true, _ -> read_select_query_from_xmls stog elt q subs
  | false, [] -> q
  | false, _ -> { q with query = keep_pcdata subs }
;;

let exec_select stog elt env query =
  try
    let dataset = dataset () in
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
        apply_select_sols env stog elt query sols
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

let fun_rdf_select stog elt_id elt env args subs =
  let query = build_select_query stog elt env args subs in
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
          (namespaces ()) s
        in
        { query with query = s }
  in
  exec_select stog elt env query
;;

let rules_rdf_select stog elt_id elt =
  let rules = Stog_html.build_base_rules stog elt_id elt in
  (("", "rdf-select"), fun_rdf_select stog elt_id elt) :: rules
;;

let rules_rdf_load stog elt_id elt =
  [ ("", "rdf"), (fun _ _ _ -> []) ;
    ("", "rdf-load"), (rule_load_graph "rdf-load" stog elt) ;
  ];;

let () = Stog_plug.register_level_fun 200 make_graph;;
let () = Stog_plug.register_level_fun_on_elt_list 201 output_graph;;
let () = Stog_plug.register_level_fun 202 (Stog_html.compute_elt rules_rdf_load);;
let () = Stog_plug.register_level_fun 220 (Stog_html.compute_elt rules_rdf_select);;




module Cache =
  struct
    type t = string
    let name = plugin_name
    let load elt xml =
      let stog = Stog_plug.stog () in
      let (g,_) = create_graph ~elt stog in
      let base = Rdf_iri.of_uri (Rdf_uri.of_neturl (Stog_html.elt_url stog elt)) in
      Rdf_xml.from_string g ~base xml ;
      add_elt_graph elt g

    let store elt =
      let stog = Stog_plug.stog () in
      let g =
        try Str_map.find (Stog_types.string_of_human_id elt.elt_human_id) !graph_by_elt
        with Not_found ->
            fst (create_graph ~elt stog)
      in
      let namespaces = namespaces () in
      Rdf_xml.to_string ~namespaces g
  end;;

let () = Stog_plug.register_cache (module Cache);;
