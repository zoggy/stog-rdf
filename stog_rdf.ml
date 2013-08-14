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
  Handle [<rdf>] nodes to create an RDF graph for the site.
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

let rdf_uri s =
  try Rdf_uri.uri s
  with e ->
      let msg = Printf.sprintf "While making uri from %S: %s" s
        (Printexc.to_string e)
      in
      failwith msg
;;

let out_file = ref "graph.rdf";;
let namespaces = ref None;;
let read_namespaces stog =
  let module CF = Config_file in
  let group = new CF.group in
  let ns = new CF.list_cp
    (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers) ~group
    ["namespaces"] [] "pairs (uri, name) specifying namespaces"
  in
  let graph_file = new CF.string_cp ~group ["graph_file"] !out_file
    "name of main graph output file"
  in
  let rc_file = rc_file stog in
  group#read rc_file;
  group#write rc_file;
  out_file := graph_file#get;
  List.fold_left
    (fun acc (uri, name) -> (rdf_uri uri, name) :: acc)
    [ Rdf_uri.of_neturl stog.Stog_types.stog_base_url, "site" ;
      Rdf_rdf.rdf_ "", "rdf" ;
    ]
    ns#get
;;

let namespaces stog =
  match !namespaces with
    Some ns -> ns
  | None ->
      let ns = read_namespaces stog in
      namespaces := Some ns;
      ns
;;

let build_ns_map namespaces =
  let pred uri name uri2 name2 =
    name = name2 && not (Rdf_uri.equal uri uri2)
  in
  let f map (uri, name) =
    try
      let name2 = Rdf_uri.Urimap.find uri map in
      let msg = Printf.sprintf "%S is already associated to name %S"
        (Rdf_uri.string uri) name2
      in
      failwith msg
    with
      Not_found ->
        if Rdf_uri.Urimap.exists (pred uri name) map then
          (
           let msg = Printf.sprintf
             "Namespace %S already associated to an url different from %S"
             name (Rdf_uri.string uri)
           in
           failwith msg
          )
        else
          Rdf_uri.Urimap.add uri name map
  in
  List.fold_left f Rdf_uri.Urimap.empty namespaces
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
        (Rdf_uri.string p, s)
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
    let ns = Rdf_uri.Urimap.fold
      (fun uri pref map -> Rdf_xml.SMap.add pref uri map)
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
      let g = Rdf_graph.open_graph (Rdf_uri.of_neturl stog.Stog_types.stog_base_url) in
      let namespaces = namespaces stog in
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
      let named =
        Str_map.fold
          (fun _ g acc -> (g.Rdf_graph.name (), g) :: acc)
          !graph_by_elt []
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
    try Rdf_uri.uri (List.assoc ("","subject") atts)
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
        (("", Rdf_uri.string Rdf_rdf.rdf_resource), uri) :: atts
  in
  let state = {
      Rdf_xml.subject = Some (Rdf_node.Uri subject) ;
      predicate = None ;
      xml_base = Rdf_uri.of_neturl stog.Stog_types.stog_base_url ;
      xml_lang = None ;
      datatype = None ;
      namespaces = gstate.Rdf_xml.gnamespaces ; (*Rdf_uri.Urimap.empty ;*)
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
        Rdf_uri.of_neturl uri
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
  let g = Rdf_graph.open_graph (Rdf_uri.of_neturl base_url) in
  let namespaces = namespaces stog in
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
  let namespaces = namespaces stog in
  Str_map.iter (fun _ g_elt -> Rdf_graph.merge g g_elt) !graph_by_elt;
  let dot = Rdf_dot.dot_of_graph ~namespaces g in
  Stog_misc.file_of_string ~file:"/tmp/graph.dot" dot;
  let out_file = Filename.concat stog.Stog_types.stog_outdir !out_file in
  Rdf_xml.to_file ~namespaces g out_file;
  Stog_plug.verbose (Printf.sprintf "RDF graph dumped into %S" out_file);
  final_graph := Some g;
  []
;;

let string_of_term = function
  Rdf_node.Uri uri -> Rdf_uri.string uri
| Rdf_node.Literal lit -> lit.Rdf_node.lit_value
| Rdf_node.Blank -> "_"
| Rdf_node.Blank_ id -> Rdf_node.string_of_blank_id id


type query_spec =
  { query : string ;
    tmpl : Xtmpl.tree list ;
    separator : Xtmpl.tree list ;
    args : Xtmpl.attribute list ;
  }

let apply_select_sol env stog elt tmpl sol =
  let atts =
    Rdf_sparql.solution_fold
      (fun name term acc -> (("", name), string_of_term term)::acc)
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
        (("", "with-xmls"), _) -> query
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
    prerr_endline ("tag=("^(fst tag)^","^(snd tag)^")");
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
  match Xtmpl.opt_arg args ~def: "false" ("", "with-xmls") = "true" with
    true -> read_select_query_from_xmls stog elt q subs
  | false ->
      match q.query with
        "" -> { q with query = keep_pcdata subs }
      | _ -> q
;;

let exec_select stog elt env query =
  try
    let dataset = dataset () in
    (*Rdf_ttl.to_file dataset.Rdf_ds.default "/tmp/rdfselect.ttl";*)
    let q = Rdf_sparql.parse_from_string query.query in
    let res = Rdf_sparql.execute
      ~base: (Rdf_uri.of_neturl stog.Stog_types.stog_base_url) dataset q
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
          (fun (uri, s) acc ->
             "PREFIX "^s^": <"^(Rdf_uri.string uri)^">\n"^acc)
          (namespaces stog) s
        in
        { query with query = s }
  in
  exec_select stog elt env query
;;

let rules_rdf_select stog elt_id elt =
  let rules = Stog_html.build_base_rules stog elt_id elt in
  (("", "rdf-select"), fun_rdf_select stog elt_id elt) :: rules
;;



let () = Stog_plug.register_level_fun 200 make_graph;;
let () = Stog_plug.register_level_fun_on_elt_list 201 output_graph;;
let () = Stog_plug.register_level_fun 220 (Stog_html.compute_elt rules_rdf_select);;




module Cache =
  struct
    type t = string
    let name = plugin_name
    let load elt xml =
      let stog = Stog_plug.stog () in
      let (g,_) = create_graph ~elt stog in
      let base = Rdf_uri.of_neturl (Stog_html.elt_url stog elt) in
      Rdf_xml.from_string g ~base xml ;
      add_elt_graph elt g

    let store elt =
      let stog = Stog_plug.stog () in
      let g =
        try Str_map.find (Stog_types.string_of_human_id elt.elt_human_id) !graph_by_elt
        with Not_found ->
            fst (create_graph ~elt stog)
      in
      let namespaces = namespaces stog in
      Rdf_xml.to_string ~namespaces g
  end;;

let () = Stog_plug.register_cache (module Cache);;
