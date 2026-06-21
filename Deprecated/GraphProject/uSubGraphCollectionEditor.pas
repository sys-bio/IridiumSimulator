unit uSubGraphCollectionEditor;

interface

Uses Classes, SysUtils, uSubgraph;

type
  TSubgraphCollectionItem = class (TCollectionItem)
    private
      FSubgraph : TSubgraph;
      procedure setSubgraph (subgraph : TSubgraph);
    published
      property subgraph: TSubgraph read FSubgraph write setSubgraph;
    end;

  TSubgraphCollection = class (TCollection)
    private
      FSubgraph: TSubgraph;
      procedure setItem (index: integer; subgraph : TSubgraph);
      function getItem (index : integer) : TSubgraph;
    public
      constructor Create (CollectionOwner: TSubgraph);
      function GetOwner: TPersistent; override;
      //procedure Update(Item: TCollectionItem); override;
      property Items[Index : integer] : TSubgraph read GetItem write SetItem; default;
    end;


implementation

constructor TSubgraphCollection.Create (CollectionOwner: TSubgraph);
begin
  inherited Create (TSubgraphCollectionItem);
  FSubgraph := CollectionOwner;
end;

function TSubgraphCollection.GetOwner: TPersistent;
begin
  Result := FSubgraph;
end;

procedure TSubgraphCollection.setItem (index: integer; subgraph : TSubgraph);
begin
  inherited setItem (Index, TCollectionItem (subgraph));
end;

function TSubgraphCollection.getItem (index : integer) : TSubgraph;
begin
  result := TSubgraph (inherited getItem (index));
end;

procedure TSubgraphCollectionItem.setSubgraph (subgraph : TSubgraph);
begin
  FSubgraph := subgraph;
end;

end.
