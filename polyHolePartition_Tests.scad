use <polyHolePartition.scad>

/*
// a case where there is bipartitition but the method find one with 10 polygons

outer = rot(45,rect(100,100,[-50,-50]));
nsq = 5;
centr = [for(i=[0:nsq-1]) [0, -7 + i*50/(nsq-1) + (1-i/(nsq-1))*-50] ];
mH = [for(i=[0:nsq-1]) revert(T(centr[i],rot(45,rect(10,10)))) ];

holeEnumeration(mH,5);


//*/

/*
// a case where there is bipartitition but the method find one with 7 polygons

outer = rect(100,100,[-50,-50]);

r1 = revert(rect(10,65,[-45,-32.5]));
d1 = revert(rot(45, rect(10,25,[15,-12.5])));
d2 = revert(rot(45, rect(10,15,[15,-6.5])));
sq = revert(rect(10,10,[-5,-5]));

mH= [r1, rot(90,r1), rot(180,r1), rot(-90,r1),
     d1, rot(90,d1), rot(180,d1), rot(-90,d1),
     sq ];

holeEnumeration(mH,6);

 //*/

/*

// a best case for the method which finds a bipartition

outer = rot(45,rect(100,100,[-50,-50]));
nsq = 5;
centr = [for(i=[0:nsq-1]) [ i*50/(nsq-1) + (1-i/(nsq-1))*-50,-5*sqrt(2)+2*i-(nsq-1)] ];
mH = [for(i=[0:nsq-1]) revert(T(centr[i],rot(45,rect(10,10)))) ];
  
difference(){
  polygon(outer);
  for(h=mH) polygon(h);
  }
  
  holeEnumeration(mH,5);

//*/

/*
// a worst case for the problem: there is no partition with less than m+1 parts

function U(a,b,w) =
  let(w=2*w)
  [ [-a,-b], [-a,b], [-a+w,b], [-a+w,-b+w], [a-w,-b+w], [a-w,b], [a,b], [a,-b] ]/2;
  
outer = rect(100,100,[-50,-50]);

mH = [ U(80,80,5), rot(180, U(55,55,5)), U(30,30,5), revert(rect(10,10,[-5,-5])) ];

//*/

//*
// another case where a bipartition exists but the method fail to find it
// even if its horizontal bias is changed to other direction
// 0s for 49 holes 4*49 vertices
// 3s for 100 holes
// 13s for 200 holes
// 52s for 400 holes
// 25 holes, 1600 vertices :  3s
// 25 holes, 3200 vertices :  9s
// 25 holes, 6400 vertices : 25s
// order estimate: O(m^2 .n.(log n) ) for m holes and n vertices


X = 100;
n = 4;
nv = 4;

echo("number of holes =", n*n);
echo("number of vertices =", 4*n*n*nv);
outer = refine(rot(-45,rect(X,X,[-X/2,-X/2])),nv);
mH = [for(i=[0:n-1], j=[0:n-1]) refine(rot(45,revert(T([(1+i)*X/(n+1)-X/n/4-X/2, (1+j)*X/(n+1)-X/n/4-X/2 ],rect(X/n/2,X/n/2)))),nv) ];
  
echo(mH[0]);

holeEnumeration(mH,6);

  
*translate([0,0,3])
for(h=concat(mH,[outer])) line([h[0]],dots=true,w=0.6);

//*/

*difference() {
  polygon(outer);
  for(h=mH) polygon(h);
}

cycles= concat(mH,[outer]);

partition = polyHolePartition(outer, mH);


/*
phedr=polyHolePartition(outer, mH, true);

verts = phedr[0];
faces = phedr[1];

echo(verts=verts);
echo(faces=faces);


translate([0,0,10])
for(i=[0:len(faces)-1]){
  pt = [for(ip=faces[i]) verts[ip] ];
  color([i/len(faces),(len(faces)-i)/len(faces),0.4+i/len(faces)/2])
  polygon(pt);
}
*/

translate([0,0,3]) polygonBorders(partition);
colorizePolygons(partition, 2*len(mH));

echo("Number of polygons =", len(partition));
echo("Are all simple polygons?", areSimple(partition));

// Helper functions for main code

// check if a polygon p generated by the method is simple
function isSimple(p) =
  [for(pi=p) if(len(search([pi],p,0)[0])>1) 0]==[];

// check if all polygons in pool generated by the method are simple
function areSimple(pool) = 
  [for(poly=pool) if(!isSimple(poly)) 0]==[];
    
function rot(a, p) =
  p*[[cos(a),sin(a)],[-sin(a),cos(a)]];

function T(d,p) = [for(pi=p) pi+d];

function rect(a,b,o=[0,0]) =
  [ o, o+[a,0], o+[a,b], o+[0,b] ];
  
function revert(l) = [for(i=[len(l)-1:-1:0]) l[i]];
  
function to3d(p) = [for(pi=p) [pi[0], pi[1], 0]];

function refine(l,n) =
  n<=0 ? l :
  let(ll = len(l))
  [for(i=[0:ll-1], k=[0:1:n-1] ) 
      (1-k/n)*l[i%ll] + k/n*l[(i+1)%ll]];

module polygonBorders(pl,col="yellow")
  for(pi=pl){
    color(col)
      line(to3d(pi),true,w=0.6);
  }

module colorizePolygons(p, n) {
  seed = floor(rands(0,10000,1)[0]);
  //echo(seed2=seed);
  //seed=6656;
  rnd = rands(0,1,3*n,seed);
  colors = [for(i=[0:n-1]) [rnd[3*i],rnd[3*i+1],rnd[3*i+1]] ];
  //if(m>=8) 
    for(i=[0:len(p)-1])
    color(colors[i])
      translate([0,0,3])
        polygon(p[i]);
   
  *color("blue")
    translate([0,0,4])
      for(i=[0:len(cycles)-1])
        line(to3d([cycles[i][0]]),dots=true,w=0.6);
  }
  
module holeEnumeration(h,size=5)
// enumeration of holes
  for(i=[0:len(h)-1]) 
    color("black")
      translate(sum_list(h[i])/len(h[i]))
        text(text=str(i),size=size,halign="center",valign="center");
  
module line(p,closed=false,dots=false,w=0.1) {
  for(i=[0:1:len(p)-(closed? 1:2)]){ 
    hull(){ translate(p[i])   sphere(w); 
            translate(p[(i+1)%len(p)]) sphere(w); 
          }
  }
  if(dots) for(pi=p) translate(pi) sphere(4*w); 
}