  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   | Name       : mergeSort for ( <comparable> * <data> ) list 
   | Paramaters : (int * 'a) list unsorted list  
   | Returns    : (int * 'a) list which is sorted
   |              I implemented the algorithm found at the merge_sort 
   |              article found on wikipedia, but I did it from just the
   |              sudo code algorithm. 
   |              
   |              No copypasta was used in this implementation. 
   |               
   | Algorithm  : en.wikipedia.org/wiki/Merge_sort                                                            
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

  local
    fun split l n  = (List.take( l, n ), List.drop( l, n ))
    fun midpoint l =  List.length l div 2
    fun merge( [],             [] )       = []
      | merge( [],          (rI,rV)::rs ) = (rI,rV)::merge( [], rs )
      | merge( (lI,lV)::ls,    [] )       = (lI,lV)::merge( ls, [] )
      | merge( (lI,lV)::ls, (rI,rV)::rs ) = case lI <= rI of true  => (lI,lV)::merge(  ls        , (rI,rV)::rs )
                                                           | false => (rI,rV)::merge( (lI,lV)::ls,  rs )
  in
    fun merge_sort [] = []
      | merge_sort l  = case List.length l = 1 of true  => l 
                                                | false => let
                                                             val (L,R) = split l ( midpoint l )
                                                           in
                                                             merge( merge_sort L, merge_sort R )
                                                           end
  end

