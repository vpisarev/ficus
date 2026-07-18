// expect: macro expansion too deep
// macro-1: direct/mutual macro recursion cannot terminate (templates are
// declarative), so it is caught by the depth limit with the expansion chain.
macro pa(e: @expr): void { pb(e) }
macro pb(e: @expr): void { pa(e) }
pa(5)
