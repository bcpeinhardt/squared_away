import { CustomType as $CustomType } from "../../../gleam.mjs";
import * as $expr from "../../../squared_away/lang/parser/expr.mjs";
import * as $typ from "../../../squared_away/lang/typechecker/typ.mjs";

export class Empty extends $CustomType {
  constructor(type_) {
    super();
    this.type_ = type_;
  }
}

export class FloatLiteral extends $CustomType {
  constructor(type_, f) {
    super();
    this.type_ = type_;
    this.f = f;
  }
}

export class StringLiteral extends $CustomType {
  constructor(type_, txt) {
    super();
    this.type_ = type_;
    this.txt = txt;
  }
}

export class IntegerLiteral extends $CustomType {
  constructor(type_, n) {
    super();
    this.type_ = type_;
    this.n = n;
  }
}

export class CellReference extends $CustomType {
  constructor(type_, key) {
    super();
    this.type_ = type_;
    this.key = key;
  }
}

export class BooleanLiteral extends $CustomType {
  constructor(type_, b) {
    super();
    this.type_ = type_;
    this.b = b;
  }
}

export class UnaryOp extends $CustomType {
  constructor(type_, op, expr) {
    super();
    this.type_ = type_;
    this.op = op;
    this.expr = expr;
  }
}

export class BinaryOp extends $CustomType {
  constructor(type_, lhs, op, rhs) {
    super();
    this.type_ = type_;
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }
}

export class Group extends $CustomType {
  constructor(type_, expr) {
    super();
    this.type_ = type_;
    this.expr = expr;
  }
}
