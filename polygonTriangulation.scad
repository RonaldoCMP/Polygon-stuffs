
/////////////////////////////////////////////////////////
//      triangulation of (almost) simple polygons      //
/////////////////////////////////////////////////////////

// Function triangulate() finds a triangulation of a polygon in a
// class of polygons a little bigger than the simple polygons.
// It generates, if possible, a list of triangles in the form of three 
// indices of the incoming vertices.
//
// Besides the list of points of the polygon to triangulate, the argument /****/
// 'best' defines which criteria to use to choose the next ear: when
// best=false (default) the first ear found is taken; when best=true, 
// a geometric criteria is used to find the best form ear avoiding very
// slim triangles when possible. When best=true, the triangulation process
// is remarkably slower.
//
// The vertices of the input polygon should pass the polyCheck() test
// to be accepted by triangulate(). Simple polygons and keyhole representation
// of polygons with holes always pass that check.
// If the input polygon doesn't pass polyCheck, a wrong and incomplete
// triangulation may be output.
//
// The points of the input polygon may have more than 2 elements. Just the
// first two elements of each point is considered by the functions as the
// point coordinates.
//
// The output list of triangles has length equal to the number of  
// incoming vertices minus 2. If the input polygon doesn't pass polyCheck 
// the function may return a shorter list and/or a wrong triangulation. 
// It uses the ear cut method to find the triangulation

// returns true when the polygon lp satisfies the conditions to be
// triangulated
function polyCheck(lp) = 
  len(lp)>=3 &&
  let( n= len(lp))
  [for(i=[0:n-3], j=[i+2:1:n-1]) 
      if( ( ( CCW(vtx(lp[i]), vtx(lp[(i+1)%n]), vtx(lp[j]))*           // edge crossings
              CCW(vtx(lp[i]), vtx(lp[(i+1)%n]), vtx(lp[(j+1)%n])) < 0 
              &&
              CCW(vtx(lp[j]), vtx(lp[(j+1)%n]), vtx(lp[i]))*
              CCW(vtx(lp[j]), vtx(lp[(j+1)%n]), vtx(lp[(i+1)%n])) < 0 
            ) 
             ||
            ( lp[i]==lp[j] &&
              CCW(vtx(lp[i]), vtx(lp[(i+1)%n]), vtx(lp[(j+n-1)%n]))*  // vertex crossing
              CCW(vtx(lp[i]), vtx(lp[(i+1)%n]), vtx(lp[(j+1)%n])) < 0
              &&
              CCW(vtx(lp[i]), vtx(lp[(j+n-1)%n]), vtx(lp[(i+n-1)%n]))*
              CCW(vtx(lp[i]), vtx(lp[(j+1)%n]),   vtx(lp[(i+n-1)%n])) < 0 
            )
          )
        )   0]==[];

// the main process
//  lp   - list of points of the polygon to triangulate
//  best - when true, uses a geometric criteria to choose the next ear
function triangulate(lp, best=false) =
  _triangulate(bdData(lp),best);
  
function _triangulate(lsp, best=false) = 
  [ for(ear  = best ? nextBestEar(lsp): nextEar(lsp), 
        tri  = earTri(ear, lsp);
      len(lsp)>=3 && ear!=undef && ear<len(lsp);
        lsp  = removeEar(ear, lsp), 
        ear  = len(lsp)>=3? best ? nextBestEar(lsp):nextEar(lsp): undef,
        tri  = len(lsp)>=3? earTri(ear, lsp) : undef )
    tri ];

// the triangle of an ear
function earTri(ear, lsp) =
  let( l = len(lsp) )
  [ ind(lsp[(ear+l-1)%l]),   
    ind(lsp[ ear]),         
    ind(lsp[(ear+1)%l]) ];
  
function removeEar(k,lsp) = 
  [for(i=[k+1:1:k-1+len(lsp)]) lsp[i%len(lsp)] ];
    
// the index of an ear 
function nextEar(lsp) =
  len([for( j=0; j<len(lsp) && !isEar(j,lsp);  j=j+1 ) j ]);
// chack if lsp[k1] is an ear
function isEar(k1, lsp) = 
   let( l  = len(lsp), 
        v0 = vtx(lsp[(k1+l-1)%l]), 
        v1 = vtx(lsp[k1]), 
        v2 = vtx(lsp[(k1+1)%l]) )
   (l==3) 
   || ( CCW(v0, v1, v2)>0 // is lsp[k1] convex ? 
         && noneInside(v0, v1, v2, k1, lsp) ); 
        
// the index of the best ear
function nextBestEar(lsp) =
  let( l = len(lsp),
       ears = [for(j=[0:len(lsp)-1]) 
                 let( v0 = vtx(lsp[j]), 
                      v1 = vtx(lsp[(j+1)%l]), 
                      v2 = vtx(lsp[(j+l-1)%l]) )
                 if( isEar(j,lsp) && v0!=v1 )  
                    [j, CCW(v0,v1,v2)/norm(v0-v1)/norm(v2-v0)/norm(v2-v1) ] ] )
  ears[ index_max([for(e=ears) e[1]]) ][0];
    
// the index of the best ear
// a more efficient implementation of the criteria of the previous function /****/
function nextBestEar(lsp) =
  let( l    = len(lsp),
       ears = [for( j    = 0, 
                    v0   = vtx(lsp[j]), 
                    v1   = vtx(lsp[(j+1)%l]), 
                    v2   = vtx(lsp[(j+l-1)%l]),
                    eval = isEar(j,lsp) ? 
                               CCW(v0,v1,v2)/norm(v0-v1)/norm(v2-v0)/norm(v2-v1): -1,
                    emax = eval,
                    jmax = eval>0 ? 0: undef;
                 j<=len(lsp) ;
                    j    = j+1, 
                    v2   = v0,
                    v0   = v1, 
                    v1   = vtx(lsp[(j+1)%l]), 
                    eval = j==len(lsp) ? -1 :
                             isEar(j,lsp) ? 
                              CCW(v0,v1,v2)/norm(v0-v1)/norm(v2-v0)/norm(v2-v1): -1,
                    jmax = eval > emax ? j: jmax,
                    emax = eval > emax ? eval: emax )
                if( j==len(lsp)-1 ) jmax ] )
  ears[0];
   
// check if there is some other vertex inside of the triangle [v0,v1,v2]
function noneInside(v0, v1, v2, k1, lsp) =
  len([for(k=[k1+2:k1+len(lsp)-2]) 
         let( v = vtx(lsp[k%len(lsp)]) )
         if( v==v0 ||  v==v1 || v==v2 ||
             ! inTri(vtx(lsp[k%len(lsp)]), v0, v1, v2) ) 0 ] )
  == len(lsp)-3;
         
// this version escape the loop as soon as a vertex
// inside [v0, v1, v2] is found
function noneInside(v0, v1, v2, k1, lsp) =
  [for( k  = k1+2,
        v  = vtx(lsp[k%len(lsp)]),
        it = inTri(v, v0, v1, v2) && !( v==v0 || v==v1 || v==v2),
        in = false;
      k<=k1+len(lsp)-1 && !in;
        k  = k+1,
        v  = vtx(lsp[k%len(lsp)]),
        in = it,
        it = inTri(v, v0, v1, v2) && !( v==v0 || v==v1 || v==v2))
      if( it ) 0 ] == [];

// true iff q is in the interior of triangle p0,p1,p2
function inTri(q, p0, p1, p2) = 
  CCW(p0,p1,q)>=0 && CCW(p1,p2,q)>=0 && CCW(p2,p0,q)>=0 ;
			
// returns 0 if a,b,c are colinear and a value >0 if the triangle [a,b,c] is CCW
function CCW(a, b, c) = (b-a).x*(c-a).y - (b-a).y*(c-a).x;

// trivial data structure to preserve original vertex indexing
function bdData(lp) = [ for(i=[0:len(lp)-1]) [ i, lp[i] ] ];
function ind(sp) = sp[0];
function vtx(sp) = [sp[1].x, sp[1].y];

//******** End of triangulation  ***********//


/*
// test example
N = 12;
seed = round(rands(0,999,1)[0]); 
//seed = 14; // fix a value to reproduce polygon
echo(seed=seed);

rays = rands(-0.5,10,N,seed=seed);

pts1 =  0.6*[ for(i=[0:N-1]) rays[i]*[cos(i*360/N), sin(i*360/N)]];
pts0 = [[0, 0], [20,0],[20,20],[0,20], [0,0], [5,5],[5,15],[15,15],[15,5],[5,5]];

pts = [for(i=[0:len(pts0)-1]) 
          i==0 || i==4 || i==5 || i==9 ? pts0[i]:pts0[i]+pts1[i]];

polygon(pts);
// this polygon may not be pass polyCheck
if(polyCheck(pts)  )
{ 
    tris = triangulate(pts);
    if(len(tris)!=len(pts)-2 ) echo("**** error ****");
    echo(polygon_length=len(pts));
    echo(number_of_triangles=len(tris));
    translate([0,0,3])
      color("yellow")
        if(tris!=[])
        for(i=[0:len(tris)-1]) {
          tri = tris[i];
          line([pts[tri[0]],pts[tri[1]]],w=0.1);
          line([pts[tri[1]],pts[tri[2]]],w=0.1);
          line([pts[tri[2]],pts[tri[0]]],w=0.1);
        }
}
else echo("inapropriate input polygon");
module line(p,closed=false,w=0.1)
  for(i=[0:len(p)-(closed? 1:2)]){ 
    hull(){ translate(p[i])   sphere(w); 
            translate(p[(i+1)%len(p)]) sphere(w); }
    translate(p[i]) sphere(4*w);
          }

//*/
