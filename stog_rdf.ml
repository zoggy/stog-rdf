(**************************************************************************)
(*              Stog-RDF                                                  *)
(*                                                                        *)
(*  Copyright (C) 2012 Maxence Guesdon. All rights reserved.              *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as               *)
(*  published by the Free Software Foundation; either version 2 of the    *)
(*  License.                                                              *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Library General Public License for more details.                  *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public             *)
(*  License along with this program; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA              *)
(*  02111-1307  USA                                                       *)
(*                                                                        *)
(*  As a special exception, you have permission to link this program      *)
(*  with the OCaml compiler and distribute executables, as long as you    *)
(*  follow the requirements of the GNU GPL in regard to all of the        *)
(*  software in the executable aside from the OCaml compiler.             *)
(*                                                                        *)
(*  Contact: Maxence.Guesdon@inria.fr                                     *)
(*                                                                        *)
(**************************************************************************)

(** RDF Plugin.
  Handle [<rdf>] nodes to create an RDF graph for the site.

  It runs through the final element body and use [<rdf pred="..." obj="..."/>]
  to add triples in the graph, using as subject:
  - the element url, or
  - (the element url)#id when we are under a node with a specified id.

  The graph is dumped into [graph.rdf] file.
*)

let file = "graph.rdf";;
let namespaces stog =
  [ Rdf_uri.uri stog.Stog_types.stog_base_url, "site" ;
    Rdf_uri.uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf" ;
  ]
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

let graph = ref None;;
let set_graph x = graph := Some x;;
let graph () =
  match !graph with
    Some x -> x
  | None ->
      let stog = Stog_plug.stog () in
      let g = Rdf_graph.open_graph (Rdf_uri.uri stog.Stog_types.stog_base_url) in
      let namespaces = namespaces stog in
      let gstate = {
          Rdf_xml.blanks = Rdf_xml.SMap.empty ;
          gnamespaces = build_ns_map namespaces ;
        }
      in
      set_graph (g, gstate);
      (g, gstate)
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
;;

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
              Printf.sprintf "%s%s"
              (Stog_html.elt_url stog elt)
              (match idopt with None -> "" | Some s -> "#"^s)
            in
            Some url
      with
        Not_found ->
          None
;;

let to_full_atts = List.map (fun (s,v) -> ("",s), v);;

let rec map_to_rdf_xml_tree = function
  Xtmpl.D s -> Rdf_xml.D s
| Xtmpl.T (tag, atts, subs) ->
    let atts = to_full_atts atts in
    map_to_rdf_xml_tree (Xtmpl.E ((("",tag), atts), subs))
| Xtmpl.E (t,subs) ->
    let subs = List.map map_to_rdf_xml_tree subs in
    Rdf_xml.E (t, subs)
;;

let parse_prop stog env g subject atts gstate subs =
  let pred =
    try List.assoc ("","pred") atts
    with Not_found -> failwith "Missing \"pred\" attribute for rdf node"
  in
  let tag = tag_of_string pred in
  let rdf_resource = get_rdf_resource stog env atts in
  let atts = List.filter
    (function
         (("","pred"),_) | (("","href"),_) | (("","obj"),_) -> false
     | _ -> true
    ) atts
  in
  let atts =
    match rdf_resource with
      None -> atts
    | Some uri -> (("rdf", "resource"), uri) :: atts
  in
  let state = {
      Rdf_xml.subject = Some (Rdf_node.Uri subject) ;
      predicate = None ;
      xml_base = Rdf_uri.uri stog.Stog_types.stog_base_url ;
      xml_lang = None ;
      datatype = None ;
      namespaces = Rdf_uri.Urimap.empty ;
    }
  in
  let subs = List.map map_to_rdf_xml_tree subs in
  let node = Rdf_xml.E ((tag, atts), subs) in
  let (gstate, _) = Rdf_xml.input_prop g state (gstate, 0) node in
  gstate
;;

let gather =
  let rec iter stog env elt_url subj_id g gstate = function
    Xtmpl.D _ -> gstate
  | Xtmpl.T (tag, atts, subs) ->
      let atts = to_full_atts atts in
      iter stog env elt_url subj_id g gstate (Xtmpl.E ((("",tag), atts), subs))
  | Xtmpl.E ((("",rdf), atts), subs) ->
      let subject =
        let uri =
          match subj_id with
            None -> elt_url
          | Some id -> elt_url ^ "#" ^ id
        in
        Rdf_uri.uri uri
      in
      parse_prop stog env g subject atts gstate subs
  | Xtmpl.E (_,subs) ->
      iter_list stog env elt_url subj_id g gstate subs
  and iter_list stog env elt_url subj_id g gstate l =
    List.fold_left (iter stog env elt_url subj_id g) gstate l
  in
  fun stog env g elt gstate xmls ->
    let elt_url = Stog_html.elt_url stog elt in
    iter_list stog env elt_url None g gstate xmls
;;

let make_graph env stog elt_id elt =
  let (g, gstate) = graph () in
  let xmls =
    match elt.Stog_types.elt_out with
      None -> elt.Stog_types.elt_body
    | Some x -> x
  in
  let x = gather stog env g elt gstate xmls in
  set_graph (g, x);
  elt
;;

let () = Stog_plug.register_level_fun 200 make_graph;;
prerr_endline "coucou";;
