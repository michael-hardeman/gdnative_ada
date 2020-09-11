# GDNative C Example

---

This is essentially a 1 for 1 translation of the [GDNative C example](https://docs.godotengine.org/en/stable/tutorials/plugins/gdnative/gdnative-c-example.html) from the Godot Documentation.

### Prerequisites

---

  Install either:
  1. [AdaCore Community](https://www.adacore.com/download) (recommended)
  2. [Gnat FSF](https://www.gnu.org/software/gnat/) (use mingw or cygwin on windows)

  Download [Godot](https://godotengine.org/download)

### Building

---

execute the following command in this folder:

```
gprbuild simple.gpr
```

This should build `lib\libsimple` (`.dll` on windows `.a` on unix)

### Running

simply open Godot and import the project.godot file
