(* ::Package:: *)
Off[CreateDirectory::ioerr];
Off[CreateDirectory::eexist];
info:={
Print[StyleForm["=====================================================","Section",FontSize->14,Black]];
Print[StyleForm["PACKAGE:","Section",FontSize->14],StyleForm[" QUANTUM CHAOS CLASSIFIER","Section",FontSize->14,Black] ];
Print[StyleForm["By: Jorge Chavez-Carlos, 2023","Section",FontSize->12,Black]];
Print[StyleForm["=====================================================","Section",FontSize->14,Black]];
Print[StyleForm["Kerr-Husimi analyzer package:","Section",FontSize->12,Black]];
Print[StyleForm["This package can classify images and data from Husimi functions or Floquet states in the Kerr-driven system.","Section",FontSize->12,Black]]
Print[StyleForm["Link to download:","Section",FontSize->10,Black]];
Print["https://..."];
Print[StyleForm["At the moment to run this package the directory called TEST was created.","Section",FontSize->12,Black]];
Print[StyleForm["You must add in the TEST directory the files that you want to classify according to the following command list:","Section",FontSize->12,Black]];
Print[StyleForm["Principal command list:","Section",FontSize->12,Black]];
Print[StyleForm["ST[state,size]:","Section",FontSize->12,Black,Bold]," Analize the Floquet or Eigenstate from information previusly calculated in the same notebook, size is about the square where the analysis will be done."];
Print[StyleForm["STD[file,size]:","Section",FontSize->12,Black,Bold]," Import the .txt Floquet or Eigenstate Data and analize the information, size refers to the square grid in phase space (q,p) where the analysis will be done"];
Print[StyleForm["FH[file]:","Section",FontSize->12,Black,Bold]," Import the Husimi Function .dat file, and analize the data."];
Print[StyleForm["IM[image]:","Section",FontSize->12,Black,Bold]," Analize the Husimi function from one .png image."];
Print[StyleForm["Subfunctions command list:","Section",FontSize->12,Black]];
Print[StyleForm["info:","Section",FontSize->12,Black,Bold]," Displays information about the package"];
Print[StyleForm["case:","Section",FontSize->12,Black,Bold]," Gives the classification between regular or chaotic"];
Print[StyleForm["prob:","Section",FontSize->12,Black,Bold]," Gives information about probabilities of being regular or chaotic"];
Print[StyleForm["fig:","Section",FontSize->12,Black,Bold]," Returns the plot-matrix for the Husimi function into an image of 50x50 resolution pixels"];
Print[StyleForm["fig0:","Section",FontSize->12,Black,Bold]," This function is only available in the command IM[], and returns the image in gray scale for the original image selected"];
Print[StyleForm["export:","Section",FontSize->12,Black,Bold]," This function is only available in the command ST[] and STF[], and exports the Husimi funtion in one file associated with the state given."];
Print["Ready to work!"];
Print[StyleForm["=====================================================","Section",FontSize->14,Black]]}[[1]];
c=Uncompress[Import["https://raw.githubusercontent.com/NuclearGeorge/KERR/main/package-ai/KERR_AI.txt"]];
SetDirectory[NotebookDirectory[]];
files=FileNames[];
If[Total[Table[If[files[[i]]=="TEST",1,0],{i,1,Length[files]}]]==0,CreateDirectory["TEST"]];
Clear[files];
SetDirectory["TEST"];

ST[a_,xL_]:={
file=a;
dat={Re@#,Im@#}&@file;
If[Chop[Total[dat[[2]]]]==0,dat=dat[[1]],dat=dat[[1]]+I dat[[2]]];
dim=Length[dat];\!\(\*
TagBox[
RowBox[{" ", 
RowBox[{
RowBox[{
RowBox[{"xx", "=", "xL"}], ";"}], " ", 
RowBox[{"Rmax", "=", 
RowBox[{
RowBox[{"-", "3.940525689180031`"}], "+", 
RowBox[{"1.3760292069233022`", " ", 
SqrtBox[
RowBox[{"0.75", "dim"}]]}]}]}]}]}],
Short[#, 2]& ]\);

xxMax=Rmax Cos[Pi/4.];
yy=xx;
xii=-xx;
xff=xx;
yii=-yy;
yff=yy;
Nx=50.;
Ny=Nx;
dx=(xff-xii)/(Nx-1);
dy=(yff-yii)/(Ny-1);
grid=Flatten[Table[{i,j},{i,xii,xff,dx},{j,yii,yff,dy}],1];
logi[i_]:=Sum[Log[j],{j,1,i}];
l=dim;
lc=l;
ECs[\[Alpha]_]=Table[(-(Abs[\[Alpha]]^2)/2+i Log[\[Alpha]]-0.5logi[i]),{i,0,dim- 1,1}];

If[xx<xxMax,
HUs={};
SetSharedVariable[HUs];
ParallelDo[ 
qi=grid[[ii,1]];
pi=grid[[ii,2]];
EC = If[qi == 0. && 
     pi == 0., {Flatten[{1, Table[0, {i, 2, l}]}]}, {Ec = 
      ECs[(qi + I pi)/Sqrt[2]][[1 ;; l]];
     (*Trucation of numerical noise*)
     Do[If[Re[Ec[[i]]] <= -37, Ec[[i]] = -37], {i, 1, Length[Ec]}];
     Chop[Exp[Ec]]}][[1]];
EVV=dat;
cks0=Chop[EC . EVV];
HuF= Abs[cks0]^2;
Norma =Total[HuF];
AppendTo[HUs,{qi,pi,HuF}];,{ii,1,Length[grid]}];
HUs=Sort[HUs];
dat=HUs[[1;;-1,3]];
name="Dim_"<> ToString[dim]<>"_SizeSquare_"<> ToString[xx];
dataTEST=dat/Max[dat];
case=c[dataTEST];
prob=c[dataTEST,"Probabilities"];
fig=Show[MatrixPlot[Transpose[Partition[dataTEST,50]]],ImageSize->Small];
export:=Export["Hus_"<>ToString[name]<>".dat",HUs];
Clear[dataTEST,dat];
,Print["Please select Square-Size less: ",xxMax];
];
}[[1]];



STF[a_,xL_]:={
file=a;
dat={Re@#,Im@#}&@Uncompress@Import@file;
If[Chop[Total[dat[[2]]]]==0,dat=dat[[1]],dat=dat[[1]]+I dat[[2]]];
dim=Length[dat];\!\(\*
TagBox[
RowBox[{" ", 
RowBox[{
RowBox[{
RowBox[{"xx", "=", "xL"}], ";"}], " ", 
RowBox[{"Rmax", "=", 
RowBox[{
RowBox[{"-", "3.940525689180031`"}], "+", 
RowBox[{"1.3760292069233022`", " ", 
SqrtBox[
RowBox[{"0.75", "dim"}]]}]}]}]}]}],
Short[#, 2]& ]\);

xxMax=Rmax Cos[Pi/4.];
yy=xx;
xii=-xx;
xff=xx;
yii=-yy;
yff=yy;
Nx=50.;
Ny=Nx;
dx=(xff-xii)/(Nx-1);
dy=(yff-yii)/(Ny-1);
grid=Flatten[Table[{i,j},{i,xii,xff,dx},{j,yii,yff,dy}],1];
logi[i_]:=Sum[Log[j],{j,1,i}];
l=dim;
lc=l;
ECs[\[Alpha]_]=Table[(-(Abs[\[Alpha]]^2)/2+i Log[\[Alpha]]-0.5logi[i]),{i,0,dim- 1,1}];

If[xx<xxMax,
HUs={};
SetSharedVariable[HUs];
ParallelDo[ 
qi=grid[[ii,1]];
pi=grid[[ii,2]];
EC = If[qi == 0. && 
     pi == 0., {Flatten[{1, Table[0, {i, 2, l}]}]}, {Ec = 
      ECs[(qi + I pi)/Sqrt[2]][[1 ;; l]];
     (*Trucation of numerical noise*)
     Do[If[Re[Ec[[i]]] <= -37, Ec[[i]] = -37], {i, 1, Length[Ec]}];
     Chop[Exp[Ec]]}][[1]];
EVV=dat;
cks0=Chop[EC . EVV];
HuF= Abs[cks0]^2;
Norma =Total[HuF];
AppendTo[HUs,{qi,pi,HuF}];,{ii,1,Length[grid]}];
HUs=Sort[HUs];
dat=HUs[[1;;-1,3]];
name=ToString[file]<>"_Dim_"<> ToString[dim]<>"_SizeSquare_"<> ToString[xx];
dataTEST=dat/Max[dat];
case=c[dataTEST];
prob=c[dataTEST,"Probabilities"];
fig=Show[MatrixPlot[Transpose[Partition[dataTEST,50]]],ImageSize->Small];
export:=Export["Hus_"<>ToString[name]<>".dat",HUs];
Clear[dataTEST,dat];
,Print["Please select Square-Size less: ",xxMax];
];
}[[1]];

FH[a_]:={
dat=Chop[Import[a][[1;;-1,3]]];
dataTEST=dat/Max[dat];
case=c[dataTEST];
prob=c[dataTEST,"Probabilities"];
fig=Show[MatrixPlot[Transpose[Partition[dataTEST,50]]],ImageSize->Small];
Clear[dataTEST,dat];}[[1]];

IM[a_]:={
fig=Import[a];
fig=ColorConvert[fig,"Grayscale"];
dataFig=Chop[ImageData[fig]];
If[Length[Dimensions[dataFig]]==3,dataFig=dataFig[[1;;-1,1;;-1,1]]];
ns=IntegerPart[Dimensions[dataFig]/50.];
dataF=Table[squarei=Flatten[dataFig[[(ns[[1]]i)+1;;ns[[1]](i+1)+1,(ns[[2]]j)+1;;ns[[2]](j+1)+1]],1];
Total[squarei]/Length[squarei],{j,0,49},{i,0,49}];
dataTEST=Flatten[dataF];
dataTEST=Table[(dataTEST[[i]]-Min[dataTEST])/(Max[dataTEST]-Min[dataTEST]),{i,1,Length[dataTEST]}];
case=c[dataTEST];
prob=c[dataTEST,"Probabilities"];
fig=Show[MatrixPlot[Transpose[Partition[dataTEST,50]]],ImageSize->Small];
fig0=Image[dataFig,ImageSize->Small];
Clear[dataTEST];
Clear[dataFig,dataF];}[[1]];
