type target =
    | Twenty
    | Nineteen
    | Eighteen
    | Seventeen
    | Sixteen
    | Fifteen
    | Bullseye;

  let allTargets = [
    Twenty,
    Nineteen,
    Eighteen,
    Seventeen,
    Sixteen,
    Fifteen,
    Bullseye
  ];

  let renderTarget: target => string =
    t =>
      switch t {
      | Twenty => "20"
      | Nineteen => "19"
      | Eighteen => "18"
      | Seventeen => "17"
      | Sixteen => "16"
      | Fifteen => "15"
      | Bullseye => "BULL"
      };

  let value: target => int =
    t =>
      switch t {
      | Twenty => 20
      | Nineteen => 19
      | Eighteen => 18
      | Seventeen => 17
      | Sixteen => 16
      | Fifteen => 15
      | Bullseye => 25
      };

  type player =
    | PlayerOne
    | PlayerTwo;

  let otherPlayer: player => player =
    x =>
      switch x {
      | PlayerOne => PlayerTwo
      | PlayerTwo => PlayerOne
      };

  type gameState = list((player, target));

  type hitsState =
    | Unhit
    | OneHit
    | TwoHits
    | Closed;

  let renderHitState: hitsState => ReasonReact.reactElement =
    hs =>
      switch hs {
      | Unhit => ReasonReact.stringToElement("");
      | OneHit => ReasonReact.stringToElement("\\");
      | TwoHits => ReasonReact.stringToElement("X");
      | Closed => <i className="times circle outline icon" />;
      };

  module Derived: {
    type t;
    let empty: t;
    let afterHit: (t, (player, target)) => t;
    let score: (t, player) => int;
    let targetState: (t, player, target) => hitsState;
  } = {
    module PlayerMap =
      Map.Make(
        {
          type t = player;
          let compare = Pervasives.compare;
        }
      );
    module TargetMap =
      Map.Make(
        {
          type t = target;
          let compare = Pervasives.compare;
        }
      );
    let targetZero: TargetMap.t(hitsState) =
      List.fold_left(
        (tm, t) => TargetMap.add(t, Unhit, tm),
        TargetMap.empty,
        allTargets
      );
    type t = {
      scores: PlayerMap.t(int),
      targetStates: PlayerMap.t(TargetMap.t(hitsState))
    };
    let empty: t = {
      scores:
        PlayerMap.empty
        |> PlayerMap.add(PlayerOne, 0)
        |> PlayerMap.add(PlayerTwo, 0),
      targetStates:
        PlayerMap.empty
        |> PlayerMap.add(PlayerOne, targetZero)
        |> PlayerMap.add(PlayerTwo, targetZero)
    };
    let score = (t, p) => t.scores |> PlayerMap.find(p);
    let targetState = (t, p, tg) =>
      t.targetStates |> PlayerMap.find(p) |> TargetMap.find(tg);
    let addPoints: (player, int, t) => t =
      (p, pts, t) => {
        let newScores = t.scores |> PlayerMap.add(p, score(t, p) + pts);
        {...t, scores: newScores};
      };
    let addTargetState: (player, target, hitsState, t) => t =
      (p, tg, hs, t) => {
        let newTargetMap =
          t.targetStates |> PlayerMap.find(p) |> TargetMap.add(tg, hs);
        {...t, targetStates: t.targetStates |> PlayerMap.add(p, newTargetMap)};
      };
    let afterHit: (t, (player, target)) => t =
      (t, (p, tg)) => {
        let (scoreDelta, newHs) =
          switch (targetState(t, p, tg)) {
          | Unhit => (0, OneHit);
          | OneHit => (0, TwoHits);
          | TwoHits => (0, Closed);
          | Closed => {
            let eligible = targetState(t, otherPlayer(p), tg) !== Closed;
            (eligible ? value(tg) : 0, Closed);
          };
          };
        t |> addPoints(p, scoreDelta) |> addTargetState(p, tg, newHs);
      };
  };

  let s2e = ReasonReact.stringToElement;

  let component = ReasonReact.reducerComponent("App");

  let make = _children => {
    ...component,
    initialState: () => Derived.empty,
    reducer: (action, state) =>
      ReasonReact.Update(Derived.afterHit(state, action)),
    render: self => {
      let rows =
        allTargets
        |> List.map(target => {
             let hitStateCell = p =>
               <td className="selectable">
                 <a href="#" onClick=(_e => self.send((p, target)))>(renderHitState(Derived.targetState(self.state, p, target)))</a>
               </td>;
             <tr>
               (hitStateCell(PlayerOne))
               <td className="target"> (s2e(renderTarget(target))) </td>
               (hitStateCell(PlayerTwo))
             </tr>;
           })
        |> Array.of_list;
      <div>
        <table className="ui celled table">
          <thead>
            <tr>
              <th> (s2e("Player One " ++ string_of_int(Derived.score(self.state, PlayerOne)))) </th>
              <th> (s2e("Target")) </th>
              <th> (s2e("Player Two " ++ string_of_int(Derived.score(self.state, PlayerTwo)))) </th>
            </tr>
          </thead>
          (ReasonReact.createDomElement("tbody", ~props={"className": "tb"}, rows))
          <tfoot />
        </table>
      </div>;
    }
  };