/*
* polyHoles converts a polygon expressed by one outer border
* and a list of internal hole borders in one polygon expressed
* by just one border by introducing two-way bridges connecting
* those borders. The output list will be a proper concatenation
* of subsequences of the incoming borders and will contain all the
* incoming vertices. So, the length of the output list will be
* the sum of the lengths of the incoming borders.
*
* The output polygon is not simple. Some of its edges appear twice
* in opposing directions when the border is travelled. Its interior
* however has no self-intersections.
*
* Restrictions
* ------------
* It is supposed the outer border to be CCW and each hole border to be CW.
* No self-intersections are allowed in any border cycle nor
* intersections between any two borders.
* The hole borders should be inside the cycle of the outer border.
* None of those conditions are checked and the run may crash if
* anyone of them fails.

* References
* ----------
* The method behind the function is based on the one found in the following 
* paper:
*
*  https://www.geometrictools.com/Documentation/TriangulationByEarClipping.pdf
*
* by David Eberly. His method converts a polygon expressed by one outer 
* border and a list of internal hole borders in one polygon expressed
* by just one border by introducing two-way bridges connecting
* those borders. The resulting polygon is good enough for calling OpenSCAD 
* polygon() and to be linear_extruded but it does not fit the requirements 
* for rendering a face the polyhedron primitive.

* The polyHoles arguments are:
*   outer - the list of the outer polygon 2D vertices
*   mH    - the list of the holes expressed as a list of 2D vertices
*   pdata - boolean controlling the kind of the function output    
* The other function arguments should not to be changed from their default 
* values.
* The function output is the keyhole polygon in one of two formats: 
*   - a list of the polygon vertices when pdata==false
*   - the same polygon in polyhedron data format, that is,
*     a pair of the polygon list and a list with the only face of it.

*/

function polyHoles(outer, mH, pdata=true, extrem=undef, n=0) =
  extrem == undef ? 
    polyHoles(outer, mH, pdata,  _extremes(mH), 0) :
    let( extr = extrem[n], // hole point with max. X
         hole = mH[extr[0]], 
         ipt  = extr[1],
         brdg = _bridge(hole[ipt], outer) ,
         err  = assert(brdg!=undef, "Error: check input polygon restrictions"),
         l    = len(outer),
         lh   = len(hole),
         // the new polyg
         npoly = [ for(i=[brdg:brdg+l]) outer[i%l]  ,
                   for(i=[ipt:ipt+lh])  hole[i%lh] ])
    n==len(mH)-1? 
      pdata ?
        [ npoly, [[for(i=[0:len(npoly)-1]) i]] ] :
        npoly : 
      polyHoles(npoly, mH, pdata, extrem, n+1);          
          
// the vertex right extreme for each hole descendent sorted by x values
function _extremes(l) =
  let( xvals = [for(i=[0:len(l)-1]) [for(j=[0:len(l[i])-1]) l[i][j].x ] ] )
  qsort( [for(i=[0:len(l)-1], k=[index_max(xvals[i])] )
              [i, k, l[i][k].x] ],-2);
      
// find a bridge between point pt
// (in the interior of poly outer) and outer
// return the index of a vertex in outer where the bridge ends
function _bridge(pt, outer) =
  let( proj = _project(pt, outer),
       ind  = proj[0],
       crxp = proj[1],
       in1  = (ind+1)%len(outer) )
  let( cand = (outer[ind].x > pt.x) || (outer[in1].x <= pt.x ) ?
               // candidates: vertices of outer which are inside
               // the triangle [ pt, outer[ind], crxp ]
               [ ind, 
                 for(i=[0:len(outer)-1]) 
                  if( outer[i].y < pt.y && inTri(outer[ind], pt, outer[i], crxp) )
                    i ]:
              // candidates: vertices of outer which are inside
              // the triangle [ pt, crxp, outer[ind+1] ]
              [ (ind+1)%len(outer),
                 for(i=[0:len(outer)-1]) 
                  if( outer[i].y > pt.y && inTri(outer[i], pt, outer[in1], crxp) )
                   i ],
    minX = min([for(i=cand) outer[i].x]) )
 [ for(c=cand) if( minX==outer[c].x ) c ][0] ;

// find the intersection of a ray to the right of p
// with the closest boundary point of outer cycle
// return the outer vertex index of the found edge and the crossing point
function _project(p, outer) =
  let(  l      = len(outer),
        crxs = [ for( i =[0:len(outer)-1], po1=[outer[i]], po2=[outer[(i+1)%l]] ) 
                   if(( (po1.y<p.y) && (po2.y>=p.y) ) &&  is_CCW(p, po1, po2) )
                      [i, _crxpt(p.y, outer[i], outer[(i+1)%l])] ],
        minX   = min([for(p=crxs) p[1].x]) )
  [for(crx=crxs) if(crx[1].x==minX) crx ][0];

// the point p of a non-horizontal line through [q,s] s.t. p.y = h
function _crxpt(h,q,s) =
  q.y==h ? q : s.y==h ? s :
  let( u = (h-s.y)/(q.y-s.y) ) u*q+(1-u)*s;
  
// check if p is inside the triangle [p1,p2,p3] given in CCW order
function inTri(p, p1, p2, p3) =
  is_CCW(p1,p,p2) && is_CCW(p2,p,p3);
     
// true if c is colinear or at left of the vector b-a 
function is_CCW(a, b, c) = 
  (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x) >= 0 ;

// General purpose functions extracted from my lists.scad and used here

    function index_max(l) = search(max(l), l)[0];
    // quicksort
    function qsort(a,k=0) =
      len(a)<=1 ? a:
      let( s = k<0? -1: 1, // k<0 => descending
           k = floor(k) )
      let( a0 = a[floor(len(a)/2)],
           l  = [for(ai=a) if(s*(ai[abs(k)]-a0[abs(k)]) <  0) ai],
           e  = [for(ai=a) if(s*(ai[abs(k)]-a0[abs(k)]) == 0) ai],
           g  = [for(ai=a) if(s*(ai[abs(k)]-a0[abs(k)]) >  0) ai] )
      concat(qsort(l,k),e,qsort(g,k));
