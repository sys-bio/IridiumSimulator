unit uBuiltInModels;

interface

Uses Classes, Generics.Collections;

type
  TBuiltInModel = class
     id : string;
     displayName : string;
     modelStr : string;
     numberOfPoints : integer;
     Ymin, Ymax : double;
     Xmin, Xmax : double;
     timeEnd : string;
     constructor Create;
  end;

  TBuiltInModels = class (TList<TBuiltInModel>)
       function getBuiltInModel (id : string) : TBuiltInModel;
  end;


var
  builtInModels : TBuiltInModels;

implementation

function getMassActionThreeStepPathway : string; forward;
function getassActionTwentyStepPathway : string; forward;
function getFeedbackModel : string; forward;
function getThreeStepPathway : string; forward;
function getLorenzAttractor : string; forward;
function getBigModel : string; forward;
function getFourSpeciesMoietyCycle : string; forward;
function getSmallestHopfModel : string; forward;
function getSimplestBistableModel : string; forward;
function getJanaWolf : string; forward;
function getBistableModel_1 : string; forward;
function getHeinrichOscilModel : string; forward;
function getTauDoyleIntegralController: string; forward;


constructor TBuiltInModel.Create;
begin
  self.numberOfPoints := 100;
  self.timeEnd := '20';
  self.Xmin := 0;
  self.Xmax := 20;
  self.Ymin := 0;
  self.Ymax := 10;
end;


function TBuiltInModels.getBuiltInModel (id : string) : TBuiltInModel;
var i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
      if id = items[i].id then
         exit (items[i]);
end;


procedure loadBuiltInModels;
var model : TBuiltInModel;
begin
  builtInModels := TBuiltInModels.Create;

  model := TBuiltInModel.Create;
  model.id := 'SimpleThreeStepPathway';
  model.displayName := 'Simple Three Step Pathway';
  model.modelStr := getMassActionThreeStepPathway;
  model.Ymin := 0;
  model.Ymax := 3;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := '20.0';
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'TwentyStepPathway';
  model.displayName := 'Twenty Step Mass-action Pathway';
  model.modelStr := getassActionTwentyStepPathway;
  model.Ymin := 0;
  model.Ymax := 100;
  model.Xmax := 0.0;
  model.Xmax := 40.0;
  model.timeEnd := '40.0';
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'ThreeStepPathway';
  model.displayName := 'Enzyme Catalyzed Three Step Pathway';
  model.modelStr := getThreeStepPathway;
  model.Ymin := 0;
  model.Ymax := 3;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := '20.0';
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'ThreeStepConservedPathway';
  model.displayName := 'Two Moiety Conerved Cycle Model';
  model.modelStr := getFourSpeciesMoietyCycle;
  model.Ymin := 0;
  model.Ymax := 3;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := '20.0';
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'feedbackModel';
  model.displayName := 'Feedback Oscillator Model';
  model.modelStr := getFeedbackModel;
  model.numberOfPoints := 600;
  model.Ymin := 0;
  model.Ymax := 4;
  model.Xmin := 0;
  model.Xmax := 10;
  model.timeEnd := '10';
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'relaxationOscilHeinrich';
  model.displayName := 'Relaxation Oscillator: From Heinrich 1977 Review';
  model.modelStr := getHeinrichOscilModel;
  model.numberOfPoints := 300;
  model.Ymin := 0;
  model.Ymax := 4;
  model.Xmin := 0;
  model.Xmax := 10;
  model.timeEnd := '10';
  builtInModels.Add(model);


  model := TBuiltInModel.Create;
  model.id := 'lorenzAttractor';
  model.displayName := 'Lorenz Attractor';
  model.modelStr := getLorenzAttractor;
  model.timeEnd := '40.0';
  model.Ymin := -30;
  model.Ymax := 50;
  model.Xmin := 0;
  model.Xmax := 50;
  model.numberOfPoints := 600;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'bigModel_1';
  model.displayName := 'Example Large Model';
  model.modelStr := getBigModel;
  model.timeEnd := '20.0';
  model.Ymin := 0;
  model.Ymax := 10;
  model.Xmin := 0;
  model.Xmax := 20;
  model.numberOfPoints := 400;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'SimpleBistableModel';
  model.displayName := 'Simple Bistable Model';
  model.modelStr := getBistableModel_1;
  model.timeEnd := '30.0';
  model.Ymin := 0;
  model.Ymax := 4;
  model.Xmin := 0;
  model.Xmax := 30;
  model.numberOfPoints := 400;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'smallestBistable';
  model.displayName := 'Smallest Bistable Model: Thomas Wilhelm';
  model.modelStr := getSimplestBistableModel;
  model.timeEnd := '30.0';
  model.Ymin := 0;
  model.Ymax := 7;
  model.Xmin := 0;
  model.Xmax := 30;
  model.numberOfPoints := 400;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'smallestHopf';
  model.displayName := 'Smallest Hopf Model: Wilhelm and Heinrich';
  model.modelStr := getSmallestHopfModel;
  model.timeEnd := '30.0';
  model.Ymin := 0;
  model.Ymax := 4;
  model.Xmin := 0;
  model.Xmax := 30;
  model.numberOfPoints := 400;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'JanaWolf';
  model.displayName := 'Jana Wolf Glycolytic Model';
  model.modelStr := getJanaWolf;
  model.timeEnd := '4.0';
  model.Ymin := 0;
  model.Ymax := 4;
  model.Xmin := 0;
  model.Xmax := 4;
  model.numberOfPoints := 400;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'Integral Controller';
  model.displayName := 'Tau/Doyle Integral Controller';
  model.modelStr := getTauDoyleIntegralController;
  model.timeEnd := '120.0';
  model.Ymin := 0;
  model.Ymax := 4;
  model.Xmin := 0;
  model.Xmax := 80;
  model.numberOfPoints := 400;
  builtInModels.Add(model);
end;


function getFourSpeciesMoietyCycle : string;
begin
  result := sLineBreak + '// Reactions:' +  sLineBreak +

  'J0: E + S1 -> ES; k1*E*S1' + sLineBreak +
  'J1: S2 -> S1; k2*S2' + sLineBreak +
  'J2: ES -> E + S2; k3*ES' + sLineBreak + sLineBreak +

  '// Species initializations:' + sLineBreak +
  'E = 5;' + sLineBreak +
  'S1 = 6;' + sLineBreak +
  'S2 = 0' + sLineBreak + sLineBreak +

  '// Variable initializations:' + sLineBreak +
  'k1 = 0.1' + sLineBreak +
  'k2 = 0.4' + sLineBreak +
  'k3 = 0.9';
end;


function getMassActionThreeStepPathway : string;
begin
  result := sLineBreak +

  '// A simple three step pathway using' + sLineBreak +
  '// mass-action kinetics.' + sLineBreak + sLineBreak +

  '// Reactions:' + sLineBreak +

  'J0: $Xo -> S1; k1*Xo' + sLineBreak +
  'J1: S1 -> S2; k2*S1' + sLineBreak +
  'J2: S2 -> ; k3*S2' + sLineBreak + sLineBreak +

  '// Species initializations:' + sLineBreak +
  'Xo = 5;' + sLineBreak +
  'S1 = 0;' + sLineBreak +
  'S2 = 0' + sLineBreak + sLineBreak +

  '// Variable initializations:' + sLineBreak +
  'k1 = 0.1' + sLineBreak +
  'k2 = 0.4' + sLineBreak +
  'k3 = 0.9';
end;


function getThreeStepPathway : string;
begin
  result := sLineBreak +

  '// A simple three step pathway using' + sLineBreak +
  '// Michaelis-Menten kinetics.' + sLineBreak + sLineBreak +

  '// Reactions:' +  sLineBreak +

  'J0: $Node0 -> Node1; (J0_Vmax/J0_Km1)*(Node0 - Node1/J0_Keq)/(1 + Node0/J0_Km1 + Node1/J0_Km2);' + sLineBreak +
  'J1: Node1 -> Node2; (J1_Vmax/J1_Km1)*(Node1 - Node2/J1_Keq)/(1 + Node1/J1_Km1 + Node2/J1_Km2);' + sLineBreak +
  'J2: Node2 -> $Node3; (J2_Vmax/J2_Km1)*(Node2 - Node3/J2_Keq)/(1 + Node2/J2_Km1 + Node3/J2_Km2);' + sLineBreak + sLineBreak +

  '// Species initializations:' + sLineBreak +
  'Node1 = 0;' + sLineBreak +
  'Node2 = 0;' + sLineBreak +
  'Node0 = 10;' + sLineBreak +
  'Node3 = 0;' + sLineBreak +  sLineBreak +

  '// Variable initializations:' + sLineBreak +
  'J0_Vmax = 1;' + sLineBreak +
  'J0_Km1 = 0.4;' + sLineBreak +
  'J0_Keq = 1;' + sLineBreak +
  'J0_Km2 = 1;' + sLineBreak +
  'J1_Vmax = 1;' + sLineBreak +
  'J1_Km1 = 0.8;' + sLineBreak +
  'J1_Keq = 1.5;' + sLineBreak +
  'J1_Km2 = 1;' + sLineBreak +
  'J2_Vmax = 1;' + sLineBreak +
  'J2_Km1 = 0.232;' + sLineBreak +
  'J2_Keq = 1.6;' + sLineBreak +
  'J2_Km2 = 1;';
end;


function getLorenzAttractor : string;
begin
  result := sLineBreak + '// Example of solving ODEs using' + sLineBreak +

  '// using the Lorenz attractor' + sLineBreak

  + '// See https://en.wikipedia.org/wiki/Lorenz_system ' + sLineBreak + sLineBreak

  + '-> u; -sigma*(u - v);' + sLineBreak
  + '-> v; rho*u - v - u*w;' + sLineBreak
  + '-> w; -beta*w + u*v;' + sLineBreak + sLineBreak

  + '// Variable initializations' + sLineBreak
  + 'u = 0' + sLineBreak
  + 'v = 1' + sLineBreak
  + 'w = 1.05' + sLineBreak + sLineBreak

  + '// Parameter initializations' + sLineBreak
  + 'sigma = 10' + sLineBreak
  + 'rho = 28' + sLineBreak
  + 'beta = 2.667'
end;


function getFeedbackModel : string;
begin
  result := sLineBreak +

  '// A negative-feeback oscillator' + sLineBreak +
  '// I think this orginally came from a' + sLinebreak +
  '// model by Athel Cornish-Bowden' + sLinebreak + sLinebreak +

  '// Reactions:' + sLineBreak +
  'J0: $X0 => S1; VM1*(X0 - S1/Keq1)/(1 + X0 + S1 + S4^h)' + sLineBreak +
  'J1: S1 => S2; (10*S1 - 2*S2)/(1 + S1 + S2)' + sLineBreak +
  'J2: S2 => S3; (10*S2 - 2*S3)/(1 + S2 + S3)' + sLineBreak +
  'J3: S3 => S4; (10*S3 - 2*S4)/(1 + S3 + S4)' + sLineBreak +
  'J4: S4 => $X1; V4*S4/(KS4 + S4)' + sLineBreak +  sLineBreak +

  '// Species initializations:' + sLineBreak +
  'S1 = 0' + sLineBreak +
  'S2 = 0' + sLineBreak +
  'S3 = 0' + sLineBreak +
  'S4 = 0' + sLineBreak +
  'X0 = 10' + sLineBreak +
  'X1 = 0' + sLineBreak + sLineBreak +

  '// Variable initializations:' + sLineBreak +
  'VM1 = 10' + sLineBreak +
  'Keq1 = 10' + sLineBreak +
  'h = 10' + sLineBreak +
  'V4 = 2.5' + sLineBreak +
  'KS4 = 0.5'
end;


function getBigModel : string;
begin
    result :=  sLinebreak +

    '// This is a randomly generated model that' + sLineBreak +
    '// uses simple mass-action kinetics' + sLineBreak + sLineBreak +

    '// Reactions'+ sLineBreak +
    'J0: S15 -> $S17; E0*(k0*S15 - k0r*S17)'+ sLineBreak +
    'J1: S3 -> S0 + S12; E1*(k1*S3 - k1r*S0*S12)'+ sLineBreak +
    'J2: $S10 -> S8; E2*(k2*S10 - k2r*S8)'+ sLineBreak +
    'J3: S9 -> S8 + S13; E3*(k3*S9 - k3r*S8*S13)'+ sLineBreak +
    'J4: $S10 + S2 -> $S17; E4*(k4*S10*S2 - k4r*S17)'+ sLineBreak +
    'J5: $S1 + S0 -> $S4; E5*(k5*S1*S0 - k5r*S4)'+ sLineBreak +
    'J6: S5 -> $S14 + S9; E6*(k6*S5 - k6r*S14*S9)'+ sLineBreak +
    'J7: S18 + S12 -> S5; E7*(k7*S18*S12 - k7r*S5)'+ sLineBreak +
    'J8: $S7 + S16 -> S2; E8*(k8*S7*S16 - k8r*S2)'+ sLineBreak +
    'J9: S6 -> S5 + S0; E9*(k9*S6 - k9r*S5*S0)'+ sLineBreak +
    'J10: $S10 + S19 -> S5; E10*(k10*S10*S19 - k10r*S5)'+ sLineBreak +
    'J11: S8 -> S18; E11*(k11*S8 - k11r*S18)'+ sLineBreak +
    'J12: S6 -> S13 + S16; E12*(k12*S6 - k12r*S13*S16)'+ sLineBreak +
    'J13: S2 + S15 -> S6 + S9; E13*(k13*S2*S15 - k13r*S6*S9)'+ sLineBreak +
    'J14: S3 -> S13; E14*(k14*S3 - k14r*S13)'+ sLineBreak +
    'J15: S13 -> S5; E15*(k15*S13 - k15r*S5)'+ sLineBreak +
    'J16: S3 -> S19; E16*(k16*S3 - k16r*S19)'+ sLineBreak +
    'J17: S0 -> S3 + S15; E17*(k17*S0 - k17r*S3*S15)'+ sLineBreak +
    'J18: S3 -> $S14 + $S17; E18*(k18*S3 - k18r*S14*S17)'+ sLineBreak +
    'J19: $S7 + $S1 -> $S11; E19*(k19*S7*S1 - k19r*S11)'+ sLineBreak + sLineBreak +

    '// Species initializations'+ sLineBreak +
    'S0 = 5'+ sLineBreak +
    'S2 = 2'+ sLineBreak +
    'S3 = 3'+ sLineBreak +
    'S5 = 1'+ sLineBreak +
    'S6 = 3'+ sLineBreak +
    'S8 = 2'+ sLineBreak +
    'S9 = 5'+ sLineBreak +
    'S12 = 1'+ sLineBreak +
    'S13 = 4'+ sLineBreak +
    'S15 = 6'+ sLineBreak +
    'S16 = 6'+ sLineBreak +
    'S18 = 3'+ sLineBreak +
    'S19 = 4'+ sLineBreak +
    'S1 = 4'+ sLineBreak +
    'S4 = 4'+ sLineBreak +
    'S7 = 3'+ sLineBreak +
    'S10 = 6'+ sLineBreak +
    'S11 = 2'+ sLineBreak +
    'S14 = 1'+ sLineBreak +
    'S17 = 1'+ sLineBreak +

    '// Variable initializations'+ sLineBreak +
    'E0 = 1;'+ sLineBreak +
    'k0 = 0.828420614'+ sLineBreak +
    'k0r = 0.69125685'+ sLineBreak +
    'E1 = 1;'+ sLineBreak +
    'k1 = 0.708821521'+ sLineBreak +
    'k1r = 0.63838456'+ sLineBreak +
    'E2 = 1;'+ sLineBreak +
    'k2 = 0.118288544' + sLineBreak +
    'k2r = 0.21911200'+ sLineBreak +
    'E3 = 1;'+ sLineBreak +
    'k3 = 0.249635736'+ sLineBreak +
    'k3r = 0.64338269'+ sLineBreak +
    'E4 = 1;'+ sLineBreak +
    'k4 = 0.148271098'+ sLineBreak +
    'k4r = 0.88154458'+ sLineBreak +
    'E5 = 1;'+ sLineBreak +
    'k5 = 0.9772751915'+ sLineBreak +
    'k5r = 0.344055979'+ sLineBreak +
    'E6 = 1;'+ sLineBreak +
    'k6 = 0.754618665'+ sLineBreak +
    'k6r = 0.68985893'+ sLineBreak +
    'E7 = 1;'+ sLineBreak +
    'k7 = 0.92086958'+ sLineBreak +
    'k7r = 0.9789804'+ sLineBreak +
    'E8 = 1;'+ sLineBreak +
    'k8 = 0.4925311695'+ sLineBreak +
    'k8r = 0.032997413'+ sLineBreak +
    'E9 = 1;'+ sLineBreak +
    'k9 = 0.80320391'+ sLineBreak +
    'k9r = 0.0041010'+ sLineBreak +
    'E10 = 1;'+ sLineBreak +
    'k10 = 0.05353786'+ sLineBreak +
    'k10r = 0.1268534'+ sLineBreak +
    'E11 = 1;'+ sLineBreak +
    'k11 = 0.1727146'+ sLineBreak +
    'k11r = 0.367270'+ sLineBreak +
    'E12 = 1;'+ sLineBreak +
    'k12 = 0.4561613'+ sLineBreak +
    'k12r = 0.247974'+ sLineBreak +
    'E13 = 1;'+ sLineBreak +
    'k13 = 0.20598231'+ sLineBreak +
    'k13r = 0.709211'+ sLineBreak +
    'E14 = 1;'+ sLineBreak +
    'k14 = 0.57478598'+ sLineBreak +
    'k14r = 0.2114866'+ sLineBreak +
    'E15 = 1;'+ sLineBreak +
    'k15 = 0.70746493'+ sLineBreak +
    'k15r = 0.7310762'+ sLineBreak +
    'E16 = 1;'+ sLineBreak +
    'k16 = 0.783348675'+ sLineBreak +
    'k16r = 0.78463660'+ sLineBreak +
    'E17 = 1;'+ sLineBreak +
    'k17 = 0.973314149'+ sLineBreak +
    'k17r = 0.58769631'+ sLineBreak +
    'E18 = 1;'+ sLineBreak +
    'k18 = 0.98747636'+ sLineBreak +
    'k18r = 0.9768005'+ sLineBreak +
    'E19 = 1;'+ sLineBreak +
    'k19 = 0.767613157'+ sLineBreak +
    'k19r = 0.85874833';
end;



function getSmallestHopfModel : string;
begin
  result := sLinebreak + '// Smallest chemical reaction system with Hopf bifurcation' + sLineBreak +
  '// Wilhelm T, Heinrich R.' + sLineBreak +
  '// Smallest chemical reaction system with Hopf bifurcation.' + sLineBreak +
  '// Journal of mathematical chemistry. 1995 Feb;17(1):1-4.' + sLineBreak + sLineBreak +

  '// Species:' + sLineBreak +
  'species X, Y, Z' + sLineBreak + sLineBreak +

  '// Reactions' + sLineBreak +
  'J0: X + $A -> 2 X; k1*X*A' + sLineBreak +
  'J1: X + Y -> $A + Y; k2*X*Y' + sLineBreak +
  'J2: X -> Z; k3*X;' + sLineBreak +
  'J3: Z -> Y; k4*Z;' + sLineBreak +
  'J4: Y ->; k5*Y;' + sLineBreak + sLineBreak +

  '// Species initializations' + sLineBreak +
  'X = 2.5' + sLineBreak +
  'Y = 2.5' + sLineBreak +
  'Z = 2.5' + sLineBreak +
  'A = 1' + sLineBreak + sLineBreak +

  // Variable initializations' + sLineBreak +
  'k1 = 3.2' + sLineBreak +
  'k2 = 1' + sLineBreak +
  'k3 = 1' + sLineBreak +
  'k4 = 1' + sLineBreak +
  'k5 = 1';
end;


function getJanaWolf : string;
begin
  result := '// Jana_WolfGlycolysis' + sLineBreak +

  '// Effect of cellular interaction on glycolytic oscillations' + sLineBreak +
  '// in yeast: a theoretical investigation.' + sLineBreak +
  '// Wolf J, Heinrich R.' + sLineBreak +
  '// Biochem. J. 2000 Jan; 345 Pt 2: 321-334' + sLineBreak +  sLineBreak +

  '// Species' + sLineBreak +
  'species Glucose, fructose_1_6_bisphosphate;' + sLineBreak +
  'species glyceraldehyde_3_phosphate, glycerate_3_phosphate;' + sLineBreak +
  'species pyruvate, Acetyladehyde, External_acetaldehyde;' + sLineBreak +
  'species ATP, ADP, NAD, NADH;' + sLineBreak +
  'species External_glucose, ethanol, Glycerol;' + sLineBreak +
  'species Sink;' + sLineBreak + sLineBreak +

  '// Reactions' + sLineBreak +
  'J0: $External_glucose => Glucose; J0_inputFlux;' + sLineBreak +
  'J1: Glucose + 2 ATP => fructose_1_6_bisphosphate + 2 ADP; J1_k1*Glucose*ATP*(1/(1 + (ATP/J1_Ki)^J1_n));' + sLineBreak +
  'J2: fructose_1_6_bisphosphate => glyceraldehyde_3_phosphate + glyceraldehyde_3_phosphate; J2_J2_k*fructose_1_6_bisphosphate;' + sLineBreak +
  'J3: glyceraldehyde_3_phosphate + NADH => NAD + $Glycerol; J3_J3_k*glyceraldehyde_3_phosphate*NADH;' + sLineBreak +
  'J4: glyceraldehyde_3_phosphate + ADP + NAD => ATP + glycerate_3_phosphate + NADH; (J4_kg*J4_kp*glyceraldehyde_3_phosphate*NAD*ADP - J4_ka*J4_kk*glycerate_3_phosphate*ATP*NADH)/(J4_ka*NADH + J4_kp*ADP);' + sLineBreak +
  'J5: glycerate_3_phosphate + ADP => ATP + pyruvate; J5_J5_k*glycerate_3_phosphate*ADP;' + sLineBreak +
  'J6: pyruvate => Acetyladehyde; J6_J6_k*pyruvate;' + sLineBreak +
  'J7: Acetyladehyde + NADH => NAD + $ethanol; J7_J7_k*Acetyladehyde*NADH;' + sLineBreak +
  'J8: Acetyladehyde => External_acetaldehyde; J8_J8_k1*Acetyladehyde - J8_J8_k2*External_acetaldehyde;' + sLineBreak +
  'J9: ATP => ADP; J9_J9_k*ATP;' + sLineBreak +
  'J10: External_acetaldehyde => $Sink; J10_J10_k*External_acetaldehyde;' + sLineBreak + sLineBreak +

  '// Species initializations' + sLineBreak +
  'Glucose = 0;' + sLineBreak +
  'fructose_1_6_bisphosphate = 0;' + sLineBreak +
  'glyceraldehyde_3_phosphate = 0;' + sLineBreak +
  'glycerate_3_phosphate = 0;' + sLineBreak +
  'pyruvate = 0;' + sLineBreak +
  'Acetyladehyde = 0;' + sLineBreak +
  'External_acetaldehyde = 0;' + sLineBreak +
  'ATP = 3;' + sLineBreak +
  'ADP = 1;' + sLineBreak +
  'NAD = 0.5;' + sLineBreak +
  'NADH = 0.5;' + sLineBreak +
  'External_glucose = 0;' + sLineBreak +
  'ethanol = 0;' + sLineBreak +
  'Glycerol = 0;' + sLineBreak +
  'Sink = 0;' + sLineBreak + sLineBreak +

  '// Variable initializations' + sLineBreak +
  'J0_inputFlux = 50;' + sLineBreak +
  'J1_k1 = 550;' + sLineBreak +
  'J1_Ki = 1;' + sLineBreak +
  'J1_n = 4;' + sLineBreak +
  'J2_J2_k = 9.8;' + sLineBreak +
  'J3_J3_k = 85.7;' + sLineBreak +
  'J4_kg = 323.8;' + sLineBreak +
  'J4_kp = 76411.1;' + sLineBreak +
  'J4_ka = 57823.1;' + sLineBreak +
  'J4_kk = 23.7;' + sLineBreak +
  'J5_J5_k = 80;' + sLineBreak +
  'J6_J6_k = 9.7;' + sLineBreak +
  'J7_J7_k = 2000;' + sLineBreak +
  'J8_J8_k1 = 375;' + sLineBreak +
  'J8_J8_k2 = 375;' + sLineBreak +
  'J9_J9_k = 28;' + sLineBreak +
  'J10_J10_k = 80;' + sLineBreak +
  'J2_k = 9.8;' + sLineBreak +
  'J3_k = 85.7;' + sLineBreak +
  'J5_k = 80;' + sLineBreak +
  'J6_k = 9.7;' + sLineBreak +
  'J7_k = 2000;' + sLineBreak +
  'J8_k1 = 375;' + sLineBreak +
  'J8_k2 = 375;' + sLineBreak +
  'J9_k = 28;' + sLineBreak +
  'J10_k = 80;'
end;


function getBistableModel_1 : string;
begin
  result := '' + sLineBreak
  + '// Simple bistable model (non-mass-action)' + sLineBreak + sLineBreak
  + '// Set S1 to 0 or S1 to 10 to observe the two stable states' + sLineBreak
  + '// To find the unstable state, set S1 = 3.4863321029603696' + sLineBreak
  + '// To examine the stabilty, change to the steady-state tab' + sLineBreak
  + '// and observe the eigenvalues (+ve = unstable)' + sLineBreak + sLineBreak

  + '// Reactions' + sLineBreak
  + 'J0: $X0 -> S1; X0*(32 + (S1/0.75)^3.2)/(1 + (S1/4.3)^3.2)' +  sLineBreak
  + 'J1: S1 ->; k1*S1;' + sLineBreak  + sLineBreak

  + '// Species initializations' + sLineBreak
  + 'S1 = 0' + sLineBreak
  + 'X0 = 0.1' + sLineBreak + sLineBreak

  + '// Parameter initialization' + sLineBreak
  + 'k1 = 3.2' + sLineBreak;
end;


function getHeinrichOscilModel : string;
begin
  result := '' + sLineBreak
  + '// Oscillator (Heinrich model)' + sLineBreak
  + '// From the review: ' + sLineBreak
  + '// Metabolic Regulation and Mathematical Models' + sLineBreak
  + '// R.HEINRICH, S.M.RAPOPORT, T.A.RAPOPORT' + sLineBreak
  + '// In Progress in Biophysics and Molecular Biology' + sLineBreak
  + '// 1977, Vol 32, p1-82' + sLineBreak + sLineBreak

  + '// Reactions:' + sLineBreak
  + 'J0: $X0 -> S1; v0' + sLineBreak
  + 'J1: S1 -> ; k3*S1' + sLineBreak
  + 'J2: S1 -> S2; (k1*S1 - k_1*S2)*(1 + c*S2^q)' + sLineBreak
  + 'J3: S2 ->; k2*S2' + sLineBreak + sLineBreak

  + '// Species initializations' + sLineBreak
  + 'S1 = 0; S2 = 1; X0 = 1' + sLineBreak + sLineBreak

  + '// Variable initializations' + sLineBreak
  + 'v0 = 8; k3 = 0' + sLineBreak
  + 'k1 = 1; k_1 = 0' + sLineBreak
  + 'c = 1;  q = 3' + sLineBreak
  + 'k2 = 5;'
end;

function getSimplestBistableModel : string;
begin
  result := '' + sLineBreak
  + '// Smallest mass-action based models that shows bistability' + sLineBreak
  + '// From:' + sLineBreak
  + '// The smallest chemical reaction system with bistability' + sLineBreak
  + '// Thomas Wilhelm' + sLineBreak
  + '// BMC Systems Biology' + sLineBreak
  + '// Vol 3(90), 2009' + sLineBreak + sLineBreak

  + '$S + Y -> 2X; k1*S*Y' + sLineBreak
  + '2X -> X + Y; k2*X*X' + sLineBreak
  + 'X + Y -> Y; k3*X*Y' + sLineBreak
  + 'X ->; k4*X;' + sLineBreak + sLineBreak

  + 'k1 = 8; k2 = 1' + sLineBreak
  + 'k3 = 1; k4 = 1.5' + sLineBreak
  + 'S = 1' + sLineBreak + sLineBreak

  + '// Set X = 2.75 to get the lower steady-state' + sLineBreak
  + '// X = 2.75' + sLineBreak

  + '// Set X = 3 to get the upper steady-state' + sLineBreak
  + 'X = 3' + sLineBreak + sLineBreak

  + '// You can also do a parameter scan ' + sLineBreak
  + '// using init([X]) from' + sLineBreak
  + '// 0.1 to 6, using 15 values';
end;


function getTauDoyleIntegralController: string;
begin
  result := '' + sLineBreak +

  '// Sample model' + sLineBreak +
  '// Integral controller from Tau and Doyle' + sLineBreak + sLineBreak +

  '$Xo -> S1; k1*Xo/(1 + P)' + sLineBreak +
  'S1 -> ; k2*S1' + sLineBreak + sLineBreak +

  'S1 -> P; k3*S1' + sLineBreak +
  'P -> ; v1' + sLineBreak + sLineBreak +

  'v1 = 0.3' + sLineBreak +

  'k1 = 0.1; k2 = 0.3' + sLineBreak +
  'k3 = 0.23; Xo = 20' + sLineBreak + sLineBreak +

  'at time > 40: k2 = k2*3' + sLineBreak +
  'at time > 80: k2 = 0.3' + sLineBreak;
end;


function getassActionTwentyStepPathway : string;
begin
  result := '' + sLineBreak +

  '// Twenty Step Mass-Action Linear Chain' + sLineBreak + sLineBreak +

  'J1: $Xo -> S1; k10*Xo - k11*S1;' + sLineBreak +
  'J2: S1 -> S2; k20*S1 - k21*S2;' + sLineBreak +
  'J3: S2 -> S3; k30*S2 - k31*S3;' + sLineBreak +
  'J4: S3 -> S4; k40*S3 - k41*S4;' + sLineBreak +
  'J5: S4 -> S5; k50*S4 - k51*S5;' + sLineBreak +
  'J6: S5 -> S6; k60*S5 - k61*S6;' + sLineBreak +
  'J7: S6 -> S7; k70*S6 - k71*S7;' + sLineBreak +
  'J8: S7 -> S8; k80*S7 - k81*S8;' + sLineBreak +
  'J9: S8 -> S9; k90*S8 - k91*S9;' + sLineBreak +
  'J10: S9 -> S10; k100*S9 - k101*S10;' + sLineBreak +
  'J11: S10 -> S11; k110*S10 - k111*S11;' + sLineBreak +
  'J12: S11 -> S12; k120*S11 - k121*S12;' + sLineBreak +
  'J13: S12 -> S13; k130*S12 - k131*S13;' + sLineBreak +
  'J14: S13 -> S14; k140*S13 - k141*S14;' + sLineBreak +
  'J15: S14 -> S15; k150*S14 - k151*S15;' + sLineBreak +
  'J16: S15 -> S16; k160*S15 - k161*S16;' + sLineBreak +
  'J17: S16 -> S17; k170*S16 - k171*S17;' + sLineBreak +
  'J18: S17 -> S18; k180*S17 - k181*S18;' + sLineBreak +
  'J19: S18 -> S19; k190*S18 - k191*S19;' + sLineBreak +
  'J20: S19 -> $X1; k200*S19 - k201*X1;' + sLineBreak + sLineBreak +

  'k10 = 4.32;  k11 = 0.36' + sLineBreak +
  'k20 = 0.37;  k21 = 0.46' + sLineBreak +
  'k30 = 5.01;  k31 = 0.86' + sLineBreak +
  'k40 = 2.22;  k41 = 0.85' + sLineBreak +
  'k50 = 1.29;  k51 = 0.83' + sLineBreak +
  'k60 = 2.25;  k61 = 0.37' + sLineBreak +
  'k70 = 0.9;  k71 = 0.25' + sLineBreak +
  'k80 = 0.28;  k81 = 0.75' + sLineBreak +
  'k90 = 2.81;  k91 = 0.41' + sLineBreak +
  'k100 = 2.30;  k101 = 0.05' + sLineBreak +
  'k110 = 0.50;  k111 = 0.17' + sLineBreak +
  'k120 = 0.90;  k121 = 0.70' + sLineBreak +
  'k130 = 0.98;  k131 = 0.46' + sLineBreak +
  'k140 = 4.22;  k141 = 0.75' + sLineBreak +
  'k150 = 0.71;  k151 = 0.44' + sLineBreak +
  'k160 = 1.33;  k161 = 0.11' + sLineBreak +
  'k170 = 0.33;  k171 = 0.21' + sLineBreak +
  'k180 = 2.21;  k181 = 0.67' + sLineBreak +
  'k190 = 3.84;  k191 = 0.45' + sLineBreak +
  'k200 = 4.19;  k201 = 0.24' + sLineBreak + sLineBreak +
  'Xo = 10.00' + sLineBreak +
  'X1 = 0' + sLineBreak +  sLineBreak +
  'S1 = 0; S2 = 0; S3 = 0; S4 = 0;' + sLineBreak +
  'S5 = 0; S6 = 0; S7 = 0; S8 = 0;' + sLineBreak +
  'S9 = 0; S10 = 0; S11 = 0; S12 = 0;' + sLineBreak +
  'S13 = 0; S14 = 0; S15 = 0; S16 = 0;' + sLineBreak +
  'S17 = 0; S18 = 0; S19 = 0' + sLineBreak;
end;

initialization
  loadBuiltInModels;
end.
