unit uBuiltInModels;

interface

Uses Classes, Generics.Collections;

type
  TBuiltInModel = class
     id : string;
     DisplayName : string;
     ModelStr : string;
     numberOfPoints : integer;
     Ymin, Ymax : double;
     Xmin, Xmax : double;
     timeEnd : double;
     constructor Create;
  end;

  TBuiltInModels = class (TList<TBuiltInModel>)
       function GetBuiltInModel (id : string) : TBuiltInModel;
  end;


var
  BuiltInModels : TBuiltInModels;

implementation

function getDefaultModel : string; forward;
function getFourStepClosedMassActionPathway : string; forward;
function getMassActionThreeStepPathway : string; forward;
function getMassActionReversibleThreeStepPathway : string; forward;
function getMassActionTwentyStepPathway : string; forward;
function getFeedbackModel : string; forward;
function getThreeStepPathway : string; forward;
function getLorenzAttractor : string; forward;
function getLargeModel : string; forward;
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
  self.timeEnd := 20;
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
  model.id := 'DefaultModel';
  model.displayName := 'Default Model';
  model.modelStr := getDefaultModel;
  model.Ymin := 0;
  model.Ymax := 8;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := 20.0;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'SimpleThreeStepPathway';
  model.displayName := 'Three Step Closed Mass-Action Pathway';
  model.modelStr := getMassActionThreeStepPathway;
  model.Ymin := 0;
  model.Ymax := 3;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := 20.0;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'SimpleFiveStepPathway';
  model.displayName := 'Five Step Closed Mass-Action Pathway';
  model.modelStr := getFourStepClosedMassActionPathway;
  model.Ymin := 0;
  model.Ymax := 3;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := 20.0;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'SimpleThreeStepReversiblePathway';
  model.displayName := 'Three Step Reversible Mass-action Pathway';
  model.modelStr := getMassActionReversibleThreeStepPathway;
  model.Ymin := 0;
  model.Ymax := 8;
  model.Xmax := 0.0;
  model.Xmax := 40.0;
  model.timeEnd := 40.0;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'TwentyStepPathway';
  model.displayName := 'Twenty Step Mass-action Pathway';
  model.modelStr := getMassActionTwentyStepPathway;
  model.Ymin := 0;
  model.Ymax := 100;
  model.Xmax := 0.0;
  model.Xmax := 40.0;
  model.timeEnd := 40.0;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'ThreeStepPathway';
  model.displayName := 'Enzyme Catalyzed Three Step Pathway';
  model.modelStr := getThreeStepPathway;
  model.Ymin := 0;
  model.Ymax := 3;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := 20.0;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'ThreeStepConservedPathway';
  model.displayName := 'Two Moiety Conerved Cycle Model';
  model.modelStr := getFourSpeciesMoietyCycle;
  model.Ymin := 0;
  model.Ymax := 3;
  model.Xmax := 0.0;
  model.Xmax := 20.0;
  model.timeEnd := 20.0;
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
  model.timeEnd := 10;
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
  model.timeEnd := 10;
  builtInModels.Add(model);


  model := TBuiltInModel.Create;
  model.id := 'lorenzAttractor';
  model.displayName := 'Lorenz Attractor';
  model.modelStr := getLorenzAttractor;
  model.timeEnd := 40.0;
  model.Ymin := -30;
  model.Ymax := 50;
  model.Xmin := 0;
  model.Xmax := 50;
  model.numberOfPoints := 600;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'bigModel_1';
  model.displayName := 'Example Large Model';
  model.modelStr := getLargeModel;
  model.timeEnd := 20.0;
  model.Ymin := 0;
  model.Ymax := 10;
  model.Xmin := 0;
  model.Xmax := 20;
  model.numberOfPoints := 400;
  builtInModels.Add(model);

  model := TBuiltInModel.Create;
  model.id := 'bigModel_2';
  model.displayName := 'Example Big Model';
  model.modelStr := getBigModel;
  model.timeEnd := 20.0;
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
  model.timeEnd := 30.0;
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
  model.timeEnd := 30.0;
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
  model.timeEnd := 30.0;
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
  model.timeEnd := 4.0;
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
  model.timeEnd := 120.0;
  model.Ymin := 0;
  model.Ymax := 4;
  model.Xmin := 0;
  model.Xmax := 80;
  model.numberOfPoints := 400;
  builtInModels.Add(model);
end;

function getDefaultModel : string;
begin
  result := '''
  // Load a model from disk, type in a model,
  // or pick one of the example models from
  // the Examples menu

  // Note // is used to indicate a comment

  // eg

  A -> B; k1*A
  B -> C; k2*B
  k1 = 0.35; k2 = 0.2
  A = 10

  // If you're not sure what to do, just
  // click the simulate button to the left
  ''';
end;


function getFourStepClosedMassActionPathway : string;
begin
  result := '''
    // Reactions:

    A -> B; k1*A
    B -> C; k2*B
    C -> D; k3*C
    D -> E; k4*D

    // Species initialization:
    A = 10

    // Parameter initialization:
    k1 = 0.35; k2 = 0.2;k3 = 0.23; k4 = 0.33
  ''';
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


function getMassActionReversibleThreeStepPathway : string;
begin
  result := sLineBreak +

  '// A simple three step reversible pathway using' + sLineBreak +
  '// mass-action kinetics.' + sLineBreak + sLineBreak +

  '// Reactions:' + sLineBreak +

  'J0: $Xo -> S1; e1*(k1*Xo - k2*S1)' + sLineBreak +
  'J1: S1 -> S2;  e2*(k3*S1 - k4*S2)' + sLineBreak +
  'J2: S2 -> ;    e3*k5*S2' + sLineBreak + sLineBreak +

  '// Species initializations:' + sLineBreak +
  'Xo = 5;' + sLineBreak +
  'S1 = 0;' + sLineBreak +
  'S2 = 0' + sLineBreak + sLineBreak +

  '// Variable initializations:' + sLineBreak +
  'e1 = 1; k1 = 0.1; k2 = 0.04' + sLineBreak +
  'e2 = 1; k3 = 0.14; k4 = 0.09' + sLineBreak +
  'e3 = 1; k5 = 0.16; Xo = 10';
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
  result := '''
  // Example of solving ODEs using

  // using the Lorenz attractor

  // See https://en.wikipedia.org/wiki/Lorenz_system

  -> u; -sigma*(u - v)
  -> v; rho*u - v - u*w
  -> w; -beta*w + u*v;

  // Variable initializations
  u = 0
  v = 1
  w = 1.05

  // Parameter initializations
  sigma = 10
  rho = 28
  beta = 2.667

  /*
    @simulate attractor: {
      timestart: 0,
      timeend: 40,
      points: 2500,
    }

    @plot: {
      source: attractor,
      x: u,
      y: v,
      title: "Chaotic attractor",
      xlabel: "u",
      ylabel: "v",
    }
  */
  ''';
end;


function getFeedbackModel : string;
begin
  result := sLineBreak +

  '// A negative-feedback oscillator' + sLineBreak +
  '// I think this originally came from a' + sLinebreak +
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


function getLargeModel : string;
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

function getBigModel : string;
begin
  Result := '''
  // Example big model, 57 species and 120 reactions
  // Generated by teUtils, build random network

  var S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19, S20, S21, S22, S23, S24, S25, S26, S27, S28, S29, S30, S31, S32, S33, S34, S35, S36, S37, S38, S39, S40, S41, S43, S44, S45, S46, S47, S48, S49, S51, S53, S54, S55, S56, S57, S58, S59
  ext S42, S50, S52;
  J0: S49 + S58 -> S1; E0*(k0*S49*S58);
  J1: S27 + S3 -> S11 + S40; E1*(k1*S27*S3);
  J2: S47 + S53 -> S24 + S41; E2*(k2*S47*S53);
  J3: S55 -> S44 + S21; E3*(k3*S55);
  J4: S2 + S53 -> S12; E4*(k4*S2*S53);
  J5: S33 + S15 -> S35 + S55; E5*(k5*S33*S15);
  J6: S32 -> S12; E6*(k6*S32);
  J7: S12 -> S7; E7*(k7*S12);
  J8: S4 -> S57 + S14; E8*(k8*S4);
  J9: S11 -> S13 + S53; E9*(k9*S11);
  J10: S34 + S41 -> S57; E10*(k10*S34*S41);
  J11: S10 + S18 -> S24; E11*(k11*S10*S18);
  J12: S28 -> S26 + S6; E12*(k12*S28);
  J13: S54 -> S27; E13*(k13*S54);
  J14: S0 -> S48 + S22; E14*(k14*S0);
  J15: S26 -> S1 + S36; E15*(k15*S26);
  J16: S58 + S39 -> S26 + S19; E16*(k16*S58*S39);
  J17: S35 -> S45 + S15; E17*(k17*S35);
  J18: S18 -> S10; E18*(k18*S18);
  J19: S4 + S46 -> S52; E19*(k19*S4*S46);
  J20: S10 + S28 -> S49; E20*(k20*S10*S28);
  J21: S27 + S9 -> S34; E21*(k21*S27*S9);
  J22: S29 + S16 -> S45 + S26; E22*(k22*S29*S16);
  J23: S46 -> S28 + S59; E23*(k23*S46);
  J24: S9 + S3 -> S46 + S28; E24*(k24*S9*S3);
  J25: S19 -> S7; E25*(k25*S19);
  J26: S56 -> S20; E26*(k26*S56);
  J27: S37 -> S54; E27*(k27*S37);
  J28: S15 + S10 -> S30; E28*(k28*S15*S10);
  J29: S30 + S27 -> S13; E29*(k29*S30*S27);
  J30: S25 + S26 -> S31; E30*(k30*S25*S26);
  J31: S36 -> S19; E31*(k31*S36);
  J32: S46 + S32 -> S15 + S12; E32*(k32*S46*S32);
  J33: S33 -> S45 + S23; E33*(k33*S33);
  J34: S43 -> S55; E34*(k34*S43);
  J35: S13 + S58 -> S35; E35*(k35*S13*S58);
  J36: S29 -> S7 + S11; E36*(k36*S29);
  J37: S2 -> S46; E37*(k37*S2);
  J38: S27 -> S52 + S53; E38*(k38*S27);
  J39: S49 + S51 -> S48; E39*(k39*S49*S51);
  J40: S31 -> S8; E40*(k40*S31);
  J41: S6 + S5 -> S38; E41*(k41*S6*S5);
  J42: S39 + S14 -> S42; E42*(k42*S39*S14);
  J43: S18 -> S59; E43*(k43*S18);
  J44: S23 + S59 -> S39; E44*(k44*S23*S59);
  J45: S9 + S36 -> S43; E45*(k45*S9*S36);
  J46: S34 -> S48 + S43; E46*(k46*S34);
  J47: S40 -> S5; E47*(k47*S40);
  J48: S0 -> S9 + S18; E48*(k48*S0);
  J49: S11 -> S25; E49*(k49*S11);
  J50: S26 -> S8 + S17; E50*(k50*S26);
  J51: S53 -> S52 + S45; E51*(k51*S53);
  J52: S37 -> S2; E52*(k52*S37);
  J53: S16 + S55 -> S2; E53*(k53*S16*S55);
  J54: S13 + S23 -> S25 + S41; E54*(k54*S13*S23);
  J55: S25 -> S28 + S33; E55*(k55*S25);
  J56: S38 + S26 -> S40 + S0; E56*(k56*S38*S26);
  J57: S18 + S29 -> S51; E57*(k57*S18*S29);
  J58: S34 -> S45; E58*(k58*S34);
  J59: S32 -> S16; E59*(k59*S32);
  J60: S34 -> S13; E60*(k60*S34);
  J61: S9 -> S8; E61*(k61*S9);
  J62: S15 -> S10; E62*(k62*S15);
  J63: S31 -> S32 + S0; E63*(k63*S31);
  J64: S40 -> S43 + S35; E64*(k64*S40);
  J65: S12 -> S9; E65*(k65*S12);
  J66: S37 -> S59; E66*(k66*S37);
  J67: S18 -> S33; E67*(k67*S18);
  J68: S10 + S34 -> S11; E68*(k68*S10*S34);
  J69: S58 + S15 -> S6 + S39; E69*(k69*S58*S15);
  J70: S8 -> S29; E70*(k70*S8);
  J71: S8 -> S35; E71*(k71*S8);
  J72: S6 -> S5 + S4; E72*(k72*S6);
  J73: S39 + S12 -> S7; E73*(k73*S39*S12);
  J74: S57 -> S29; E74*(k74*S57);
  J75: S31 -> S59 + S32; E75*(k75*S31);
  J76: S22 + S12 -> S36; E76*(k76*S22*S12);
  J77: S6 -> S40; E77*(k77*S6);
  J78: S0 + S21 -> S16; E78*(k78*S0*S21);
  J79: S39 + S7 -> S32; E79*(k79*S39*S7);
  J80: S45 + S3 -> S50; E80*(k80*S45*S3);
  J81: S37 -> S24; E81*(k81*S37);
  J82: S6 + S7 -> S28 + S53; E82*(k82*S6*S7);
  J83: S51 -> S41 + S3; E83*(k83*S51);
  J84: S33 -> S43 + S23; E84*(k84*S33);
  J85: S17 -> S44 + S50; E85*(k85*S17);
  J86: S54 + S43 -> S4; E86*(k86*S54*S43);
  J87: S30 -> S37 + S49; E87*(k87*S30);
  J88: S48 + S17 -> S22; E88*(k88*S48*S17);
  J89: S58 + S2 -> S56; E89*(k89*S58*S2);
  J90: S19 -> S9; E90*(k90*S19);
  J91: S47 + S48 -> S12 + S51; E91*(k91*S47*S48);
  J92: S1 + S16 -> S14; E92*(k92*S1*S16);
  J93: S11 + S51 -> S4; E93*(k93*S11*S51);
  J94: S51 -> S14; E94*(k94*S51);
  J95: S22 + S1 -> S17 + S26; E95*(k95*S22*S1);
  J96: S26 + S55 -> S35; E96*(k96*S26*S55);
  J97: S0 -> S3 + S58; E97*(k97*S0);
  J98: S49 + S28 -> S24; E98*(k98*S49*S28);
  J99: S41 -> S33; E99*(k99*S41);
  J100: S25 + S13 -> S1; E100*(k100*S25*S13);
  J101: S5 -> S57 + S44; E101*(k101*S5);
  J102: S46 -> S15; E102*(k102*S46);
  J103: S9 -> S18; E103*(k103*S9);
  J104: S1 + S27 -> S53; E104*(k104*S1*S27);
  J105: S25 + S11 -> S13 + S5; E105*(k105*S25*S11);
  J106: S13 -> S42; E106*(k106*S13);
  J107: S39 -> S28 + S10; E107*(k107*S39);
  J108: S26 -> S34; E108*(k108*S26);
  J109: S6 -> S47; E109*(k109*S6);
  J110: S55 + S9 -> S7 + S49; E110*(k110*S55*S9);
  J111: S29 -> S49; E111*(k111*S29);
  J112: S55 -> S54; E112*(k112*S55);
  J113: S31 -> S47 + S22; E113*(k113*S31);
  J114: S31 + S44 -> S37; E114*(k114*S31*S44);
  J115: S2 -> S51 + S10; E115*(k115*S2);
  J116: S25 -> S30; E116*(k116*S25);
  J117: S0 -> S47 + S46; E117*(k117*S0);
  J118: S48 + S20 -> S30; E118*(k118*S48*S20);
  J119: S24 -> S35 + S18; E119*(k119*S24);

  k0 = 0.7204273056630408
  k1 = 0.3585816589962262
  k2 = 0.32864569601813476
  k3 = 0.7839898798246722
  k4 = 0.35499265310412675
  k5 = 0.7713634060453071
  k6 = 0.6034473128017588
  k7 = 0.7574490909222182
  k8 = 0.6366252305328469
  k9 = 0.9043211035299757
  k10 = 0.9642780179083494
  k11 = 0.1710357804575069
  k12 = 0.5952718244393802
  k13 = 0.44293301449736644
  k14 = 0.3828027884665123
  k15 = 0.4979096497257043
  k16 = 0.9204495641982264
  k17 = 0.5465370594947166
  k18 = 0.7350876833504681
  k19 = 0.030446566357431815
  k20 = 0.042998663250359836
  k21 = 0.5142657298942758
  k22 = 0.8444270504928064
  k23 = 0.3214793171745961
  k24 = 0.5521394433320997
  k25 = 0.7960883887506761
  k26 = 0.039144593674593775
  k27 = 0.7988234713645302
  k28 = 0.9721174540228001
  k29 = 0.07074081722537473
  k30 = 0.5625058311026407
  k31 = 0.8177706944127132
  k32 = 0.6467260750401942
  k33 = 0.4734135802015429
  k34 = 0.9886486821862474
  k35 = 0.542714926272457
  k36 = 0.41601231216320755
  k37 = 0.9494626296194328
  k38 = 0.3364294055189876
  k39 = 0.43122421857020576
  k40 = 0.924235837158782
  k41 = 0.7313786823640694
  k42 = 0.00116044742482857
  k43 = 0.6606576715121988
  k44 = 0.8128895197305768
  k45 = 0.2884505066423052
  k46 = 0.9889163393177185
  k47 = 0.9564180116374158
  k48 = 0.6233279012971383
  k49 = 0.5126682385034954
  k50 = 0.26842867838371465
  k51 = 0.5136301459337319
  k52 = 0.05088183511162059
  k53 = 0.2539618894395138
  k54 = 0.7370360332457319
  k55 = 0.45809072947164553
  k56 = 0.07439760392853134
  k57 = 0.5219845436925833
  k58 = 0.8606480360412102
  k59 = 0.22398717863197293
  k60 = 0.33650921632046693
  k61 = 0.6103895789190226
  k62 = 0.4067056246780295
  k63 = 0.8090342320433925
  k64 = 0.1511225198354027
  k65 = 0.7047932748174226
  k66 = 0.05391726708507594
  k67 = 0.2829757849675142
  k68 = 0.3405703004592293
  k69 = 0.7760751325614644
  k70 = 0.7363098132251475
  k71 = 0.9231606717038963
  k72 = 0.1824052886360108
  k73 = 0.7984847449109808
  k74 = 0.7827243353366802
  k75 = 0.7626848171070998
  k76 = 0.8725925063355949
  k77 = 0.9949648146334369
  k78 = 0.5695666624811668
  k79 = 0.06815291088016351
  k80 = 0.7958869002032781
  k81 = 0.7911097373090817
  k82 = 0.14135165781618808
  k83 = 0.978481382374625
  k84 = 0.08199197635774136
  k85 = 0.8324789983578603
  k86 = 0.008621021293073183
  k87 = 0.47616908490431686
  k88 = 0.29429236373158674
  k89 = 0.5356531301765081
  k90 = 0.21101695066025428
  k91 = 0.921777342052112
  k92 = 0.7089594019880193
  k93 = 0.31023572827287793
  k94 = 0.006468268846043013
  k95 = 0.2913983670159511
  k96 = 0.5119227804191275
  k97 = 0.7395449959470209
  k98 = 0.4152618985204565
  k99 = 0.6259015043309166
  k100 = 0.4462588007490307
  k101 = 0.5035012349788025
  k102 = 0.16560338059372892
  k103 = 0.8185832096244344
  k104 = 0.669440423765693
  k105 = 0.3161875844166343
  k106 = 0.40945328057340247
  k107 = 0.04402763203031268
  k108 = 0.3732324702682509
  k109 = 0.5102200603332847
  k110 = 0.27773736413847694
  k111 = 0.05051782851275399
  k112 = 0.11193229968658525
  k113 = 0.6590860693481625
  k114 = 0.4562207562583517
  k115 = 0.7025247661313562
  k116 = 0.5839289415755025
  k117 = 0.7513383492895512
  k118 = 0.7247702302068451
  k119 = 0.9272820593566902

  E0 = 1
  E1 = 1
  E2 = 1
  E3 = 1
  E4 = 1
  E5 = 1
  E6 = 1
  E7 = 1
  E8 = 1
  E9 = 1
  E10 = 1
  E11 = 1
  E12 = 1
  E13 = 1
  E14 = 1
  E15 = 1
  E16 = 1
  E17 = 1
  E18 = 1
  E19 = 1
  E20 = 1
  E21 = 1
  E22 = 1
  E23 = 1
  E24 = 1
  E25 = 1
  E26 = 1
  E27 = 1
  E28 = 1
  E29 = 1
  E30 = 1
  E31 = 1
  E32 = 1
  E33 = 1
  E34 = 1
  E35 = 1
  E36 = 1
  E37 = 1
  E38 = 1
  E39 = 1
  E40 = 1
  E41 = 1
  E42 = 1
  E43 = 1
  E44 = 1
  E45 = 1
  E46 = 1
  E47 = 1
  E48 = 1
  E49 = 1
  E50 = 1
  E51 = 1
  E52 = 1
  E53 = 1
  E54 = 1
  E55 = 1
  E56 = 1
  E57 = 1
  E58 = 1
  E59 = 1
  E60 = 1
  E61 = 1
  E62 = 1
  E63 = 1
  E64 = 1
  E65 = 1
  E66 = 1
  E67 = 1
  E68 = 1
  E69 = 1
  E70 = 1
  E71 = 1
  E72 = 1
  E73 = 1
  E74 = 1
  E75 = 1
  E76 = 1
  E77 = 1
  E78 = 1
  E79 = 1
  E80 = 1
  E81 = 1
  E82 = 1
  E83 = 1
  E84 = 1
  E85 = 1
  E86 = 1
  E87 = 1
  E88 = 1
  E89 = 1
  E90 = 1
  E91 = 1
  E92 = 1
  E93 = 1
  E94 = 1
  E95 = 1
  E96 = 1
  E97 = 1
  E98 = 1
  E99 = 1
  E100 = 1
  E101 = 1
  E102 = 1
  E103 = 1
  E104 = 1
  E105 = 1
  E106 = 1
  E107 = 1
  E108 = 1
  E109 = 1
  E110 = 1
  E111 = 1
  E112 = 1
  E113 = 1
  E114 = 1
  E115 = 1
  E116 = 1
  E117 = 1
  E118 = 1
  E119 = 1

  S42 = 1
  S50 = 1
  S52 = 4

  S0 = 5
  S1 = 4
  S2 = 2
  S3 = 4
  S4 = 2
  S5 = 6
  S6 = 3
  S7 = 4
  S8 = 3
  S9 = 1
  S10 = 5
  S11 = 6
  S12 = 1
  S13 = 4
  S14 = 2
  S15 = 5
  S16 = 4
  S17 = 1
  S18 = 2
  S19 = 3
  S20 = 3
  S21 = 1
  S22 = 6
  S23 = 6
  S24 = 2
  S25 = 6
  S26 = 1
  S27 = 3
  S28 = 3
  S29 = 4
  S30 = 4
  S31 = 1
  S32 = 6
  S33 = 6
  S34 = 5
  S35 = 1
  S36 = 3
  S37 = 1
  S38 = 6
  S39 = 6
  S40 = 1
  S41 = 5
  S43 = 4
  S44 = 4
  S45 = 2
  S46 = 6
  S47 = 2
  S48 = 1
  S49 = 5
  S51 = 6
  S53 = 5
  S54 = 5
  S55 = 3
  S56 = 5
  S57 = 4
  S58 = 6
  S59 = 3
  ''';
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


function getMassActionTwentyStepPathway : string;
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
