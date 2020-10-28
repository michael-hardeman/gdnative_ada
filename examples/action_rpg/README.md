# Action RPG Example

---

This is a verions of [Heartbeast's Action RPG youtube tutorial](https://www.youtube.com/watch?v=mAbG8Oi-SvQ&list=PL9FzW-m48fn2SlrW0KoLT4n5egNdX-W9a).

### License

[Heartbeast's MIT Licence](https://github.com/uheartbeast/youtube-tutorials/blob/50fa9c2b8c6349a607a4dd731da1d6c2bc73f48e/LICENSE)
or see LICENSE.md

### Prerequisites

---

1. GNAT (Ada Compiler)
  * [AdaCore Community GNAT](https://www.adacore.com/download)
    A version of FSF GNAT curated by Ada Core. Free for personal users, probably the highest quality and most used Ada Compiler. Supports Windows, Linux, and Mac
  * [FSF GNAT](https://www.gnu.org/software/gnat/)
    GCC bundled open source version of GNAT. For linux use the above link, for windows use mingw or cygwin to install.
2. [Godot](https://godotengine.org/download)

### Building

---

execute the following command in this folder:

```
gprbuild action_rpg.gpr
```

This should build `lib\libaction_rpg` (`.dll` on windows `.a` on unix)

### Running

simply open Godot and import the project.godot file

