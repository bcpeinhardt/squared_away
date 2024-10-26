import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";

class Effect extends $CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
}

export function from(effect) {
  return new Effect(toList([(dispatch, _) => { return effect(dispatch); }]));
}

export function event(name, data) {
  return new Effect(toList([(_, emit) => { return emit(name, data); }]));
}

export function none() {
  return new Effect(toList([]));
}

export function batch(effects) {
  return new Effect(
    $list.fold(
      effects,
      toList([]),
      (b, _use1) => {
        let a = _use1.all;
        return $list.append(b, a);
      },
    ),
  );
}

export function map(effect, f) {
  return new Effect(
    $list.map(
      effect.all,
      (eff) => {
        return (dispatch, emit) => {
          return eff((msg) => { return dispatch(f(msg)); }, emit);
        };
      },
    ),
  );
}

export function perform(effect, dispatch, emit) {
  return $list.each(effect.all, (eff) => { return eff(dispatch, emit); });
}
