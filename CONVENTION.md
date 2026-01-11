# Coding convention 

## Packages visibility

### Internals packages
All entities within the Parchemin hierarchy must use **Fully Qualified Name (FQN)**.
- **Rule:** The use clause is strictyle prohibited for internal packages.
- **why:** Ensure architectural traceabilty and prevents naming conflicts.
- **example:** `Parchemin.Types.Memory\_Size;`

### External packages
External packages should be access via Local **Renaming (Aliases)** defined in the package body.
- **Rule:**: Use Short, uppercase aliases (2-4 characters). Avoid the use clause to prevent
namespace pollution.
- **Rationale:** Reduces visual noise while clearly separating system calls from business logic.

**Commons aliases**
| package                   | alias |
|:---                       |:---   |
| Ada.Command\_Line         | ACL   |
| Ada.Characters.Handling   | ACH   |
| Ada.IO\_Exceptions        | IOE   |
| Ada.Strings               | STR   |
[ Ada.Strings.Fixed         | STF   |
| Ada.Strings.Maps          | MAP   |
| Ada.Strings.Unbounded     | SUS   | 
| Ada.Text\_IO              | TIO   |

## Subprogram banners

### General rules

**width:** All separator lines must occupy exactly 80 columns.
**format:** The lin e start with -- and continues with - until column 80 is reached.
**Doctstring:** The GNATDoc syntaxe is required.

<p align="center">
  <img src="assets/separators.jpg" alt="Separators buddy !">
</p>

Is an example for banner in specifications files (.ads)
```
--------------------------------------------------------------------------------
--  Procedure: Handle (Overriding)
--
--  Main dispatcher for the module commands.
--
--  @param Self      The command instance. 
--  @param Path      The full path to the data source.
--  @param Callable  The subcommand string.
--  @param Start_Arg Index where subcommand's parameters begin.
--
--  @raise Program_Error if arguments are missing or invalid.a
--
--  @return See you space cowboy.
--------------------------------------------------------------------------------
```
and this for body files (.adb)
```
--------------------------------------------------------------------------------
--  Procedure: Handle (Overriding)
--------------------------------------------------------------------------------
```
