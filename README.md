This is a plugin for [Stog](http://zoggy.github.com/stog/).

It allows to easily specify RDF triples within elements,
to produce a RDF graph for the whole generated site.

## Compiling and installing

To install:

    git clone git@github.com:zoggy/stog-rdf.git
    cd stog-rdf
    make all install

(you must have [OCaml-RDF](http://ocaml-rdf.forge.ocamlcore.org/) installed)
This will install the stog-rdf package with ocamlfind.

## Usage

To use:

    stog --package stog-rdf ...

## Defining RDF triples

Stog-rdf defines a new rewrite rule associated to level 200, `rdf`.
Each `<rdf>` node corresponds to a new triple whose subject
is the current element url, or the current block id of the current
element if the `<rdf>` node is contained in a XML node with an `id`
attribute.

Another function, associated to level 201, merges all RDF
graphs into one graph and dumps this graph into a file.
The name of this file is set in the `.stog/config-rdf` file
(default is `graph.rdf`).

Examples:

    <page title="...">
      ...
      <rdf pred="..." obj="..."/>
      ...
    </page>

Subject is the element url, `pred` is the URI of the predicate
(possibly of the form `prefix:term`). `obj` is the URI of
the object.

    <page title="...">
      ...
      <rdf pred="..." href="..."/>
      ...
    </page>

Subject and predicate are as in the first example `href` is of
the form `hid[#id]` to indicate an element of the site, and eventually
a node id in this element. This is reduced to the complete URI
of the element of element#id.

    <page title="...">
       ...
       <p id="id1">
         <rdf pred="..." .../>
       </p>
    </page>

Here the subject is the URI formed by the element url and the id of the
`<p>` node, for example `http://my.example.com/foo/page1.html#id1` if
my element has hid foo/page1 and base url of the site is `http://my.example.com`.

To add the triples, the `<rdf pred="pref:term" ...>...</rdf>` node is rewritten to

    <rdf:Description rdf:about="<element-url>[#id]">
      <pref:term ...>
        ...
      </pref:term>
    </rdf:Description>

and this XML tree is parsed using OCaml-RDF RDF/XML parser to add the triples
to the element graph. If a `href` or `obj` attribute was used, the corresponding
URI is associated to the `rdf:resource` attribute in the rewritten node:

    <rdf:Description rdf:about="<element-url>[#id]">
      <pref:term rdf:resource="..." ...>
        ...
      </pref:term>
    </rdf:Description>

and the whole `<rdf:Description>` node is parsed by the RDF/XML parser.

## Namespaces

Namespaces used in RDF triples in the site must be declared in the
`.stog/config-rdf` file.

The `rdf` and `site` namespaces don't need to be specified and are
automatically handled respectively as

    rdf:http://www.w3.org/1999/02/22-rdf-syntax-ns#
    site:<site base url>

(the site base url is the one specified with `stog-site-url` in the main stog element).

## Tip

To make definition of triples even easier, one can define rules in the
element header, for example:

    <page title="..." with-contents="true">
      <isproof><rdf pred="rdf:hasType" obj="science:proof"/></isproof>
      <contents>
        ...
          <div class="proof" id="proof1">
            <isproof/>
          ...
          </div>
        ...
      </contents>
    </page>

This will add a triple

    <element-url>#proof1 rdf:hasType science:proof

