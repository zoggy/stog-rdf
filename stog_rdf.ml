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

let graph = ref None;;
let set_graph g = graph := Some g;;
let graph () =
  match !graph with
    Some g -> g
  | None ->
    let stog = Stog_plug.stog () in
    let g = Rdf_graph.open_graph (Rdf_uri.uri stog.Stog_types.stog_base_url) in
    set_graph g;
    g
;;

let gather =
  let rec iter g = function
    Xtmpl.D _ -> g
  | Xtmpl.E (((_,t),atts),subs) ->
      let atts = List.map (fun (a,v) -> (snd a,v)) atts in
      iter g (Xtmpl.T (t, atts, subs))
  | Xtmpl.T ("rdf", atts, subs) ->
      let pred =
        match Xtmpl.get_arg atts "pred" with
          None -> failwith "Missing \"pred\" attribute for rdf node"
        | Some s -> Rdf_uri.uri s
      in
      let obj =
        match Xtmpl.get_arg atts "obj" with
        | Some s -> Rdf_uri.uri s
        | None ->
            failwith "Not implemented yet"
(*            match subs with
              [Xtmpl.D s] ->
            | _ -> failwith "Missing \"pred\" attribute for rdf node"*)
      in
      assert false
  | Xtmpl.T (_,_,subs) ->
      iter_list g subs
  and iter_list g l = List.fold_left iter g l in
  iter_list
;;

let make_graph _ _ elt_id elt =
  let g = graph () in
  let xmls =
    match elt.Stog_types.elt_out with
      None -> elt.Stog_types.elt_body
    | Some x -> x
  in
  let g = gather g xmls in
  set_graph g;
  elt
;;

let () = Stog_plug.register_level_fun 200 make_graph;;