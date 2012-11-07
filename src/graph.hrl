-record(graph_node, {id,
                     props=orddict:new(),
                     neighbors=[]}).

-record(node, {id,
               props}).
