# Minimal

---

This is the smallest amount of code required for godot to load the library and have the library print a few messages in the godot console.

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
gprbuild minimal.gpr
```

This should build `lib\libminimal` (`.dll` on windows `.a` on unix)

### Running

simply open Godot and import the project.godot file
