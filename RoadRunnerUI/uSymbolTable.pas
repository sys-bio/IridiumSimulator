{ ******************************************************** }
{                                                          }
{ Copyright (c) H M Sauro 1996                             }
{ ******************************************************** }

unit uSymbolTable;

{ Binary Search Tree Object Implementation }

{ Use:
    t := TBinarySearchTree.Create;
    insert ('a', 0.0);
    insert ('b', 3.14);
    p := find_symbol ('b');    { p point to node in tree holding 'b'
    x := GetSymTabValue (p);   { return value of 'b' given the pointer
    x := FetchValue ('b');     { returns value of 'b', if 'b' not found
                                 then raises an exception
    t.free;
}


interface

uses SysUtils;

const
   NODE_LIST_LENGTH = 100;

type
   EBinSearchError = class (Exception);

   pTSymTabNode = ^TSymTabNode;
   TSymTabNode = record
                   name : string;
                   value : Extended; index : integer; { index gives order of insertion }
                   left, right : pTSymTabNode;
                 end;

   TNodeList = array[1..NODE_LIST_LENGTH] of pTSymTabNode;
   pTNodeList = ^TNodeList;

   TBinarySearchTree = class (TObject)
                        private
                          FTree : pTSymTabNode;
                          FnSymbols : integer;
                          NodeList : pTNodeList;
                          NodeListLength : integer;
                          procedure kill_NodeList;
                          procedure EnlargeNodeList;
                        public
                          constructor create;
                          destructor  destroy; override;
                          procedure Clear;
                          function  GetSymTabValue (np : pTSymTabNode) : Extended;
                          procedure SetSymTabValue (np : pTSymTabNode; value : Extended);
                          function  GetSymTabName (np : pTSymTabNode) : string;
                          function  GetnthSymbol (n : integer) : pTSymTabNode;
                          procedure SetnthSymbol (n : integer; d : double);
                          function  insert (name : string; d : Extended) : pTSymTabNode;
                          function  find_symbol (name : string) : pTSymTabNode;
                          function  FetchValue (name : string) : Extended;
                          function  GetnSymbols : integer;
                       end;


implementation


{ Recursive destruction of binary search tree, called by method clear and destroy }
procedure kill_tree (var p : pTSymTabNode);
begin
  if p <> Nil then
     begin
     kill_tree (p^.left);
     kill_tree (p^.right);
     Dispose (p);
     p := Nil;
     end;
end;


procedure TBinarySearchTree.kill_NodeList;
begin
  if NodeList <> Nil then FreeMem (NodeList, sizeof (TSymTabNode) * NodeListlength);
  NodeList := Nil;
end;


procedure TBinarySearchTree.EnlargeNodeList;
var s : integer;
begin
  s := sizeof (TSymTabNode);
  { Modifiation for D2, ReAllocMem no longer requires original size and is now a procedure !}
  ReAllocMem (NodeList, s * NodeListLength + 10);
  inc (NodeListLength, 10);
end;


constructor TBinarySearchTree.Create;
begin
  inherited Create;
  FTree := Nil; FnSymbols := 0; NodeListLength := NODE_LIST_LENGTH;
  NodeList := AllocMem (sizeof (TSymTabNode) * NODE_LIST_LENGTH);
end;


destructor TBinarySearchTree.Destroy;
begin
  kill_tree (FTree);
  kill_NodeList;
  inherited Destroy;
end;



procedure TBinarySearchTree.Clear;
begin
  kill_tree(FTree); FnSymbols := 0;
end;


procedure TBinarySearchTree.SetSymTabValue (np : pTSymTabNode; value : Extended);
begin
  np^.value := value;
end;


{ Get symbol table entry value }
function TBinarySearchTree.GetSymTabValue (np : pTSymTabNode) : Extended;
begin
  result := np^.value
end;


{ Get name in symbol table }
function TBinarySearchTree.GetSymTabName (np : pTSymTabNode) : string;
begin
  result := np^.name;
end;


function TBinarySearchTree.GetnthSymbol (n : integer) : pTSymTabNode;
begin
  result := NodeList^[n];
end;


procedure TBinarySearchTree.SetnthSymbol (n : integer; d : double);
begin
  NodeList^[n]^.value := d;
end;


{ find name in symbol table. Retun a pointer to the node if symbol found,
else return Nil to indicate failure }
function TBinarySearchTree.find_symbol (name : string) : pTSymTabNode;
var np : pTSymTabNode; cmp : integer;
begin
   { convert any upper case letters to lower case first }
   name := LowerCase (name);
   { next search symbol table for name - get the root of the tree first,
   which is kept by in a private field }
   np := Self.FTree;
   while (np <> Nil) do
         begin
         cmp := CompareText(name, np^.name);
         if cmp = 0 then   { found !}
            begin
            result := np;
            exit;
            end;

         if cmp < 0 then
            np := np^.left
         else
            np := np^.right;
         end;
  result := Nil;  { register failure to find name using Nil return code }
end;


function TBinarySearchTree.GetnSymbols : integer;
begin
  result := FnSymbols;
end;



function TBinarySearchTree.FetchValue (name : string) : Extended;
var np : pTSymTabNode;
begin
  np := find_symbol (name);    { np point to node in tree holding 'b'  }
  if np <> Nil then
     result := GetSymTabValue (np)   { return value of 'b' given the pointer  }
  else
     raise EBinSearchError.Create ('Error: Couldn''t locate symbol <' + name + '>');
end;


{ add a symbol to the table if it's a new one. Returns pointer to the new
node if a new node inserted, else returns Nil to indicate node already present }
function TBinarySearchTree.insert (name : string; d : Extended) : pTSymTabNode;
var np, NewNode : pTSymTabNode; pnp : ^pTSymTabNode;  { pointer to pointer to node ! }
    cmp : integer;
begin
   { Check if symbol already in table }
   if find_Symbol (name) = Nil then
      begin
      { ....if not enter the new symbol. Create new node }
      NewNode := New (pTSymTabNode);
      NewNode^.name := name;
      NewNode^.value := d;
      NewNode^.left := Nil; NewNode^.right := Nil;

      { Next enter new node into symbol table. Find insertion point }
      if Self.FTree = Nil then { if no table then attach to root }
         begin
         Self.FTree := NewNode;
         result := NewNode;
         FnSymbols := 1; NodeList^[FnSymbols] := result;
         end
      else
         begin
         { ...otherwise search for insertion point. pnp always points to
         a location that points to a node }
         pnp := @(Self.FTree);  { get pointer to root, note root is already a pointer! }
         np := pnp^;            { start at root first. Get the root pointer }
         while (np <> Nil) do
               begin
               cmp := CompareText(name, np^.name);
               if cmp < 0 then
                  pnp := @np^.left    { get address of new starting position }
               else
                  pnp := @np^.right;  { " }
               np := pnp^;            { get the pointer to the new starting position }
               end;
         { exit if empty leaf node pointer = nil }
         { note that pnp holds a pointer to a left/right location, the insertion point }
         pnp^ := NewNode;  { insert the new node }
         result := pnp^;
         inc (FnSymbols, 1);
         { Enter new symbol into NodeList, but check first we havn't run out of room }
         if FnSymbols > NodeListLength then EnlargeNodeList;
         NodeList^[FnSymbols] := result;
         end;
      end
   else
      result := Nil;
end;


end.

