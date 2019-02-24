use <polyHolePartition.scad>
use <sweep.scad>

// the face with holes
outer = circ(r=50,n=24); // the outer border 
mH    = cxydistr(25, 4, 3, revert(circ(12,24))); // hole borders
// build the polygon for the face with holes
holedFacePDat = polyHolePartition(outer, mH, true);
// main path
path = spiral(200,80,270,24);
// the path transforms
tpath = construct_transform_path(path);
// sweep the outer face and the holes without caps
souter = sweep_polyhedron(outer, tpath, caps=false);
shole0 = sweep_polyhedron(mH[0], tpath, caps=false);
shole1 = sweep_polyhedron(mH[1], tpath, caps=false);
shole2 = sweep_polyhedron(mH[2], tpath, caps=false);
// transform holedFacePDat to put the holed caps in proper place
lpath = len(tpath);
begCap   = tranfPdata(tpath[0], holedFacePDat) ;
endCap   = tranfPdata(tpath[lpath-1], holedFacePDat, inv=true) ;
// join evething in one polyhedron
difference() {
  lazyUnion([souter,begCap,endCap, shole0, shole1, shole2]);
  translate([-160,50,0]) cube([60,60,150]);
}

// helper funtions
function spiral(r,h,ang,n) =
  [for(i=to_(n)) [r*cos(i*ang/n), r*sin(i*ang/n),i*h/n] ];
function circ(r,n,a=360) = [for(i=to_(n)) r*[cos(i*a/n), sin(i*a/n),0] ];
function T_(p,lp) = [for(pi=lp) pi+p ];
function cxydistr(r,n,m,lp) =  [for(i=to_(m)) rotz(i*360/n, T_([0,r,0],lp) ) ];
function tranfPdata(transf, pdata, inv=false) = 
  ! inv ?
    [ transform(transf, pdata[0]), pdata[1] ] :
    [ transform(transf, pdata[0]), [for(f=pdata[1]) revert(f) ] ];
function rotz(a,lp) =
  let( T = [ [cos(a), sin(a), 0],[-sin(a),cos(a),0],[0,0,1] ] )
  lp[0][0]==undef? T*lp :[ for(pi=lp) T*pi ] ;
function to2d(p) = 
  p[0][0]==undef ? [p.x, p.y] : [for(pi=p) [pi.x, pi.y] ];
function revert(l) = !(len(l)>0)? [] : [ for(i=[len(l)-1:-1:0]) l[i] ];
function to_(n) = [0:n-1];
function transform(m, list) = [for (p=list)let( q = m*concat(p,[1]) ) to3d(q)/q[3]];
function to3d(p) = [p.x, p.y, p.z];
