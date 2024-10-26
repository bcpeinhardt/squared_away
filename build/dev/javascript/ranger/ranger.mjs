import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $iterator from "../gleam_stdlib/gleam/iterator.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import * as $order from "../gleam_stdlib/gleam/order.mjs";
import { Ok, Error, CustomType as $CustomType } from "./gleam.mjs";

class Forward extends $CustomType {}

class Backward extends $CustomType {}

export function create(validate, negate_step, add, compare) {
  let adjust_step = (a, b, step) => {
    let negated_step = negate_step(step);
    let $ = compare(a, b);
    let $1 = compare(a, add(a, step));
    let $2 = compare(a, add(a, negated_step));
    if ($ instanceof $order.Eq) {
      return new Ok(new $option.None());
    } else if ($1 instanceof $order.Eq && $2 instanceof $order.Eq) {
      return new Ok(new $option.None());
    } else if ($ instanceof $order.Lt && $1 instanceof $order.Lt) {
      return new Ok(new $option.Some([new Forward(), step]));
    } else if ($ instanceof $order.Lt && $2 instanceof $order.Lt) {
      return new Ok(new $option.Some([new Forward(), negated_step]));
    } else if ($ instanceof $order.Lt) {
      return new Error(undefined);
    } else if ($ instanceof $order.Gt && $1 instanceof $order.Gt) {
      return new Ok(new $option.Some([new Backward(), step]));
    } else if ($ instanceof $order.Gt && $2 instanceof $order.Gt) {
      return new Ok(new $option.Some([new Backward(), negated_step]));
    } else {
      return new Error(undefined);
    }
  };
  return (a, b, s) => {
    return $bool.guard(
      !validate(a) || !validate(b),
      new Error(undefined),
      () => {
        let $ = adjust_step(a, b, s);
        if ($.isOk() && $[0] instanceof $option.Some) {
          let direction = $[0][0][0];
          let step = $[0][0][1];
          return new Ok(
            $iterator.unfold(
              a,
              (current) => {
                let $1 = compare(current, b);
                if ($1 instanceof $order.Gt && direction instanceof Forward) {
                  return new $iterator.Done();
                } else if ($1 instanceof $order.Lt &&
                direction instanceof Backward) {
                  return new $iterator.Done();
                } else {
                  return new $iterator.Next(current, add(current, step));
                }
              },
            ),
          );
        } else if ($.isOk() && $[0] instanceof $option.None) {
          return new Ok($iterator.once(() => { return a; }));
        } else {
          return new Error(undefined);
        }
      },
    );
  };
}

export function create_infinite(validate, add, compare) {
  let is_step_zero = (a, s) => {
    let $ = compare(a, add(a, s));
    if ($ instanceof $order.Eq) {
      return true;
    } else {
      return false;
    }
  };
  return (a, s) => {
    return $bool.guard(
      !validate(a),
      new Error(undefined),
      () => {
        return $bool.guard(
          is_step_zero(a, s),
          (() => {
            let _pipe = $iterator.once(() => { return a; });
            return new Ok(_pipe);
          })(),
          () => {
            let _pipe = $iterator.unfold(
              a,
              (current) => {
                return new $iterator.Next(current, add(current, s));
              },
            );
            return new Ok(_pipe);
          },
        );
      },
    );
  };
}
