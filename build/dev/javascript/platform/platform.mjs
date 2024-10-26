import { CustomType as $CustomType } from "./gleam.mjs";
import { runtime as runtime_, os as os_, arch as arch_ } from "./platform_ffi.mjs";

export class Erlang extends $CustomType {}

export class Node extends $CustomType {}

export class Bun extends $CustomType {}

export class Deno extends $CustomType {}

export class Browser extends $CustomType {}

export class OtherRuntime extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Aix extends $CustomType {}

export class Darwin extends $CustomType {}

export class FreeBsd extends $CustomType {}

export class Linux extends $CustomType {}

export class OpenBsd extends $CustomType {}

export class SunOs extends $CustomType {}

export class Win32 extends $CustomType {}

export class OtherOs extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Arm extends $CustomType {}

export class Arm64 extends $CustomType {}

export class X86 extends $CustomType {}

export class X64 extends $CustomType {}

export class Loong64 extends $CustomType {}

export class Mips extends $CustomType {}

export class MipsLittleEndian extends $CustomType {}

export class PPC extends $CustomType {}

export class PPC64 extends $CustomType {}

export class RiscV64 extends $CustomType {}

export class S390 extends $CustomType {}

export class S390X extends $CustomType {}

export class OtherArch extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function runtime() {
  let $ = runtime_();
  if ($ === "erlang") {
    return new Erlang();
  } else if ($ === "node") {
    return new Node();
  } else if ($ === "bun") {
    return new Bun();
  } else if ($ === "deno") {
    return new Deno();
  } else if ($ === "browser") {
    return new Browser();
  } else {
    let runtime$1 = $;
    return new OtherRuntime(runtime$1);
  }
}

export function os() {
  let $ = os_();
  if ($ === "aix") {
    return new Aix();
  } else if ($ === "darwin") {
    return new Darwin();
  } else if ($ === "freebsd") {
    return new FreeBsd();
  } else if ($ === "linux") {
    return new Linux();
  } else if ($ === "openbsd") {
    return new OpenBsd();
  } else if ($ === "sunos") {
    return new SunOs();
  } else if ($ === "win32") {
    return new Win32();
  } else if ($.startsWith("win")) {
    return new Win32();
  } else {
    let os$1 = $;
    return new OtherOs(os$1);
  }
}

export function arch() {
  let $ = arch_();
  if ($ === "arm") {
    return new Arm();
  } else if ($ === "arm64") {
    return new Arm64();
  } else if ($ === "aarch64") {
    return new Arm64();
  } else if ($ === "x86") {
    return new X86();
  } else if ($ === "ia32") {
    return new X86();
  } else if ($ === "x64") {
    return new X64();
  } else if ($ === "x86_64") {
    return new X64();
  } else if ($ === "amd64") {
    return new X64();
  } else if ($ === "loong64") {
    return new Loong64();
  } else if ($ === "mips") {
    return new Mips();
  } else if ($ === "mipsel") {
    return new MipsLittleEndian();
  } else if ($ === "ppc") {
    return new PPC();
  } else if ($ === "ppc64") {
    return new PPC64();
  } else if ($ === "riscv64") {
    return new RiscV64();
  } else if ($ === "s390") {
    return new S390();
  } else if ($ === "s390x") {
    return new S390X();
  } else {
    let arch$1 = $;
    return new OtherArch(arch$1);
  }
}
