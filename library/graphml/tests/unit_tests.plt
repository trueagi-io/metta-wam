/* Unit tests
 * to be run from this directory
*/

:- begin_tests(lists).

:- use_module('../read_graphml').


test(basic) :-
    read_graphml:read_graphml(basic, Terms),
    Terms = [
        node{description:answer, id:n0, label:idea},
        node{description:question, id:n1, label:issue},
        edge{id:e0, label:solves, source_id:n0, target_id:n1}
    ].

test(edge_label_back) :-
    read_graphml:read_graphml(edge_label_back, Terms),
    Terms = [
        node{description:answer, id:n0, label:idea},
        node{description:question, id:n1, label:issue},
        edge{id:e0, label:'label back', source_id:n0, target_id:n1}
    ].

test(edge_nolabel) :-
    read_graphml(edge_nolabel, Terms),
    Terms = [
        node{description:answer,id:n0,label:idea},
        node{description:question,id:n1,label:issue},
        edge{id:e0,label:'',source_id:n0,target_id:n1}
    ].

test(mute_node) :-
    read_graphml:read_graphml(mute_node, Terms),
    Terms = [
        node{description:answer,id:n0,label:idea},
        node{description:question,id:n1,label:issue},
        node{description:con,id:n2,label:""},
        edge{id:e0,label:solves,source_id:n0,target_id:n1}
    ].

test(node_desc_back) :-
    read_graphml:read_graphml(node_desc_back, Terms),
    Terms = [
        node{description:answer,id:n0,label:idea},
        node{description:question,id:n1,label:issue},
        node{description:con,id:n2,label:'label back'},
        edge{id:e0,label:solves,source_id:n0,target_id:n1}
    ].

test(node_label_back) :-
    read_graphml:read_graphml(node_label_back, Terms),
    Terms = [
        node{description:answer,id:n0,label:idea},
        node{description:question,id:n1,label:issue},
        node{description:con,id:n2,label:'label back'},
        edge{id:e0,label:solves,source_id:n0,target_id:n1}    
    ].

test(node_nodesc) :-
    read_graphml:read_graphml(node_nodesc, Terms),
    Terms = [
        node{description:answer,id:n0,label:idea},
        node{description:question,id:n1,label:issue},
        node{description:"",id:n2,label:'label back'},
        edge{id:e0,label:solves,source_id:n0,target_id:n1}
    ].

test(node_nolabel) :-
    read_graphml:read_graphml(node_nolabel, Terms),
    Terms = [
        node{description:answer,id:n0,label:idea},
        node{description:question,id:n1,label:issue},
        node{description:con,id:n2,label:""},
        edge{id:e0,label:solves,source_id:n0,target_id:n1}    
    ].


:- end_tests(lists).