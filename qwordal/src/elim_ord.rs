//fn simplicial_elimination_ordering<R: Cc>(graph: &Interferences<R>) -> Vec<Tmp> {
//    let mut ordering = Vec::new();
//
//    let mut curr = graph
//        .all_nodes()
//        .filter_map(Stg::try_as_tmp)
//        .next()
//        .unwrap();
//
//    let mut weights: PriorityQueue<Tmp, usize> = graph
//        .all_nodes()
//        .filter_map(|node| match node {
//            Stg::Tmp(tmp) => Some(tmp),
//            Stg::Reg(_) => None,
//        })
//        .filter(|t| *t != curr)
//        .map(|tmp| {
//            // Count precolored nodes:
//            let weight = graph
//                .neighbors(&Stg::Tmp(tmp))
//                .filter(|nbr| matches!(nbr, Stg::Reg(_)))
//                .count();
//            (tmp, weight)
//        })
//        .collect();
//
//    while !weights.is_empty() {
//        ordering.push(curr);
//        for nbr in graph.neighbors(&Stg::Tmp(curr)) {
//            let Stg::Tmp(nbr) = nbr else { continue };
//            weights.change_priority_by(&nbr, |w| *w += 1);
//        }
//        (curr, _) = weights.pop().unwrap();
//    }
//
//    ordering.push(curr);
//
//    ordering
//}
//
