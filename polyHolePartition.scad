// General purpose functions extracted from my lists.scad and used here

    function flatten(list) = [ for (a = list, v = a) v ];
    function index_max(l) = search(max(l), l)[0];
    function sum_list(l) = len(l) > 0 ? [ for(li=l) 1 ] * l : 0;
    function accum_sum(l, offs=0, res=[]) =
        len(res) == len(l) ?
            res :
            accum_sum(l, offs+l[len(res)], concat(res, [ offs+l[len(res)] ] ));
    // comparison quicksort
    function cqsort(a,c="asc") =
      len(a)<=1 ? a:
      let( a0 = a[floor(len(a)/2)],
           l  = [for(ai=a) if(compare(ai,a0,c) <  0) ai],
           e  = [for(ai=a) if(compare(ai,a0,c) == 0) ai],
           g  = [for(ai=a) if(compare(ai,a0,c) >  0) ai] )
      concat(cqsort(l,c),e,cqsort(g,c));
    // comparison function example for cqsort
    // may be overloaded by application
    function compare(a0, a1, c) =
      c=="asc"?  len(a0)==undef ? a0-a1 : a0[0]-a1[0] :
      c=="desc"? len(a0)==undef ? a1-a0 : a1[0]-a0[0] :
      c=="lex"?  a0[0]-a1[0]==0? a0[1]-a1[1] : a0[0]-a1[0] : // lexicographic
      c==floor(c) && c>=0 && len(a0)>=0 ? a0[c]-a1[c] :      // by key ascendent
      c==floor(c) && c<0  && len(a0)>=0 ? a1[-c]-a0[-c] :    // by key descendent
      undef;


//  -----------------
//  polyHolePartition
//  -----------------
/*
* This function generates a partition of a polygon with holes such that
* each part of the partition is a simple polygon.
* The generated patition has at most m+1 parts.
* It may not be optimal in the number of the parts for the given data.

* Restrictions
* ------------
* It is supposed the outer border to be CCW and each hole border to be CW.
* No self-intersections are allowed in any border cycle nor
* intersections between any two borders.
* The hole borders should be inside the cycle of the outer border.
* None of those conditions are checked and the run may crash if
* anyone of them fails.

* Input and output
* ----------------
* The polyHolePartition() parameters are:
*   outer - the list of the outer polygon vertices in CCW order
*   mH    - the list of the holes expressed as a list of vertices in CW order
*   pdata - a boolean controlling the kind of function output
* The vertices of the input lists may have more than 2 components but
* just the first two are used. The output polygon vertices will preserve
* any additional data the input borders may contain.
* Two alternative outputs are possible from the function depending of the
* value of the argument pdata:
*   pdata==false -> the list of the polygons in the partition given by vertices
*   pdata==true  -> the list of polygons in a polyhedron input style, that is,
*                   a pair [vertices, faces]
* By default, pdata=false.

* References
* ----------
* The method behind the function was inspired by the following paper:
*
*  https://www.geometrictools.com/Documentation/TriangulationByEarClipping.pdf
*
* by David Eberly. His method converts a polygon expressed by one outer 
* border and a list of internal hole borders in one polygon expressed
* by just one border by introducing two-way bridges connecting
* those borders. The resulting polygon is good enough for calling OpenSCAD polygon()
* but it does not fit the requirements for rendering a linear_extrude of it.
*/
    
// Implementation
 
// produces a partition in simple polygons of the polygon outer 
// with holes in list mH 
// returns a list of simple polygons whose union is outer with holes in mH
function polyHolePartition(outer, mH, pdata=false) =
  // add a tag to each vertex with the index of its original cycle 
  // and the original vertex index
  let( lmH  = len(mH),
       out1 = vNodeStruct( outer, len(mH)),
       mH1  = [for(i=[0:len(mH)-1]) vNodeStruct( mH[i],i)],
       out2 = vNodeStruct(-outer, len(mH)),
       mH2  = [for(i=[0:len(mH)-1]) vNodeStruct(-mH[i],i)]
     )
  let( phc  = polyHoleComplex(out1, mH1) ,
       poly = phc[0], // vertices of the "simplified" polygon 
       br1  = phc[1], // added bridges to "simplify" it
       br0  = polyHoleComplex(out2, mH2)[1] , // additional bridges for 
                                                 // partitioning
       brd  = remDup(br1,br0),// eliminate duplications of bridges
       // insert each bridge of brd in poly
       polys = [for(  i = 0, p = [poly];
                      i<=len(brd) ;
                      b = i<=len(brd)-1? brd[i]: undef,
                      p = i<=len(brd)-1? insertBridge(b, p) : undef,
                      i = i+1
                   ) if(i==len(brd) ) each p]
      )
  ! pdata ?
     // just the polygons with original data
    [for(pl=polys)[for(p=pl) vtx(p) ] ] : 
    // polyhedron data output
    let( cyc   = concat(mH,[outer]),
         acc   = accum_sum([0, for(pi=cyc) len(pi) ]) )
    [ flatten(cyc), 
      [for(p=polys)[for(pt=p) acc[ndh(pt)]+ndv(pt) ] ] ];
        

// vNodeStruct adds node information to each vertex of a cycle (outer or hole)
// each component of the vNodeStruct is a vNode containing an original cycle 
// vertex and a node; a node is a pair identifying uniquely a vertex of a cycle
function vNodeStruct(cyc,n) = [for(i=[0:len(cyc)-1]) [ cyc[i], [n, i] ] ];
// access functions to vertex and nodes of a vNode representation
function vtx(p) = to2d(p[0]);  // 2d vertex coordinates
function nd(p)  = p[1];        // vertex node
function ndh(p) = p[1][0];     // vertex node hole
function ndv(p) = p[1][1];     // vertex index in its cycle

// access functions to bridge components
function bh(br) = br[0][0];   // begin hole index of a bridge
function eh(br) = br[1][0];   // end hole index
function bv(br) = br[0][1];   // begin vertex index
function ev(br) = br[1][1];   // end vertex index
function bn(br) = br[0];      // begin node
function en(br) = br[1];      // end node

    
// remove from br2 all bridges whose reverse connects the same 
// pair of cycles as some bridge in br1
function remDup(br1, br2) = 
  [for(b2=br2)
    if([for(b1=br1) 
          if([ eh(b1), bh(b1)]==[ bh(b2), eh(b2)] ) 0 ]==[])
      b2];

// given a list of polygons polys insert the bridge br that will 
// subdivide in two some polygon of polys; returns the new set of polygons
function insertBridge(br, polys) =
  let( tags = [for(pl=polys) [for(p=pl) p[1] ] ], // extract tags from polys
       ibv  = [for(t=tags) search([bn(br)], t, 1)[0] ], 
       iev  = [for(t=tags) search([en(br)], t, 1)[0] ],
       ipt  = [for(i=[0:len(polys)-1]) 
                 if(ibv[i]!=[] && iev[i]!=[]) 
                   each [ibv[i],iev[i],i] 
             ])
  subdivide(ipt, polys);
  
// subdivide the polygon polys[ipts[2]] in two by adding an edge between
// its vertices with indices ipts[0] and pts[1]
function subdivide(ipts, polys) =
  let( ib = ipts[0],    // bridge begin vertex
       ie = ipts[1],    // bridge end vertex
       ip = ipts[2],    
       pl = polys[ip] ) // polygon to subdivide
  [ [for(i=[ie: ib<ie? ib+len(pl): ib]) pl[i%len(pl)] ], // pl subdivision
    [for(i=[ib: ie<ib? ie+len(pl): ie]) pl[i%len(pl)] ],
    for(i=[ip+1:1:ip+len(polys)-1]) polys[i%len(polys)] // all other polygons
  ];
       
// the core of the method
// given a polygon with holes (outer, mH) find a set of m=len(mH) bridges that
// transform outer in an "almost simple" polygon P, that is, P covers the 
// polygon  without the holes in mH but it has self-interceptions in its 
// border; the output is the pair of P and the list of the added bridges 
function polyHoleComplex(outer, mH) =
  _polyHoleComplex(outer, mH, extremes(mH)); 
                  
function _polyHoleComplex(outer, mH, extrem=undef, n=0, brdgs=[]) =
    let( extr  = extrem[n],   // reference to the hole vertex
                              // with max. X among all previous holes
         hole  = mH[extr[0]], // the right extreme hole
         ibeg  = extr[1],     // index of the right extreme vertex of hole
         iend  = bridge2pt(vtx(hole[ibeg]), outer), // index of bridge end in outer
         lo    = len(outer),
         lh    = len(hole),
         brdg  = [nd(hole[ibeg]), nd(outer[iend])], // the bridge to be added 
         npoly = [ for(i=[iend:iend+lo])   outer[i%lo] , // the new polyg
                   for(i=[ibeg : ibeg+lh]) hole [i%lh] ])
    n==len(mH)-1 ? 
      [ npoly, concat(brdgs, [brdg])]:
      _polyHoleComplex(npoly, mH, extrem, n+1, concat(brdgs, [brdg]));

// the vertex right extreme for each hole descendent sorted by x values
function extremes(l) =
  let( xvals = [for(i=[0:len(l)-1]) [for(j=[0:len(l[i])-1]) vtx(l[i][j]).x ] ] )
  cqsort( [for(i=[0:len(l)-1]) 
              let( k=index_max(xvals[i]) ) 
              [i, k, vtx(l[i][k]).x] ],-2);
      
// find a bridge between the point pt (in the interior of poly outer) and
// outer return the index of a vertex in outer where the bridge ends
function bridge2pt(pt, outer) =
  let( proj = project(pt, outer), // horizontal projection of pt on outer
       ind  = proj[0],            // outer edge index of projection point
       crxp = proj[1],            // projection point
       in1  = (ind+1)%len(outer) )
  crxp == vtx(outer[ind]) ? 
    ind :
  crxp == vtx(outer[in1]) ?
    in1 :
    // outer may intercept the segment joining pt to vtx(outer[ind]) 
    // or vtx(outer[in1])
    let( cand = (vtx(outer[ind]).x > pt.x && 
                   norm(crxp - vtx(outer[in1]))
                     >1e-6*norm(vtx(outer[ind])-vtx(outer[in1])))
                 || (vtx(outer[in1]).x <= pt.x ) ?
                 // candidates: vertices of outer which are inside
                 // the triangle [ pt, outer[ind], crxp ]
                 [ ind, 
                   for(i=[0:len(outer)-1]) 
                    if( vtx(outer[i]).y < pt.y   
                        && inTri(vtx(outer[ind]), pt, vtx(outer[i]), crxp) )
                      i ]:
                // candidates: vertices of outer which are inside
                // the triangle [ pt, crxp, outer[ind+1] ]
                [ (ind+1)%len(outer),
                   for(i=[0:len(outer)-1]) 
                    if( vtx(outer[i]).y > pt.y
                        && inTri(vtx(outer[i]), pt, vtx(outer[in1]), crxp) )
                     i ],
    minX = min([for(i=cand) vtx(outer[i]).x]) )
 [ for(c=cand) if( minX==vtx(outer[c]).x ) c ][0] ;

// find the intersection of a ray to the right of p
// with the closest boundary point of outer cycle
// return the outer vertex index of the found edge and the crossing point
function project(p, outer) =
  let(  l    = len(outer),
        crxs = [ for( i =[0:len(outer)-1], po1=[vtx(outer[i])], po2=[vtx(outer[(i+1)%l])] ) 
                   if( (po1.y<p.y) && (po2.y>=p.y) &&  is_CCW(p, po1, po2) )
                      [i, crxpt(p.y, vtx(outer[i]), vtx(outer[(i+1)%l]))] ],
        minX = min([for(c=crxs) c[1].x]) )
  [for(crx=crxs) if(crx[1].x==minX) crx ][0];

// find the point p in a non-horizontal line segment [q,s] s.t. p.y = h
function crxpt(h,q,s) =
  q.y==h ? q : s.y==h ? s :
  let( u = (h-s.y)/(q.y-s.y) ) u*q+(1-u)*s;
  
// check if p is inside the triangle [p1,p2,p3] given in CCW order
function inTri(p, p1, p2, p3) =
  is_CCW(p1,p,p2) && is_CCW(p2,p,p3);

// true if c is colinear or at left of the vector b-a 
function is_CCW(a, b, c) = 
  cross(to2d(b-a), to2d(c-a)) > 0;
//  (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x) >= 0 ;
  
function to2d(p) = 
  p[0][0]==undef ? [p.x, p.y] : [for(pi=p) [pi.x, pi.y] ];

//********* end of implementation ***************//

// Additional helper functions

// check if a polygon p generated by the method is simple
function isSimple(p) =
  [for(pi=p) if(len(search([pi],p,0)[0])>1) 0]==[];

// check if all polygons in pool generated by the method are simple
function areSimple(pool) = 
  [for(poly=pool) if(!isSimple(poly)) 0]==[];
