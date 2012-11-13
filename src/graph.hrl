-record(graph_node, {id,
                     props=orddict:new(),
                     neighbors=orddict:new()}).

-record(node, {id,
               props}).
