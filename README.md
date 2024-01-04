# <img src="./docs/logo.png" alt="Fyr Logo" height="10%"> Fyr

Fyr will be a programming language specifically geared towards building games & software for the Nintendo Game Boy Advance and will have features catering to the same.

This repository will host the Fyr Compiler (Fyrc) which will support fully featured end to end compilation from Fyr source code to GBA (ARM7TDMI) machine code with minimal third-party dependencies.

While the Language Design and the Frontend are still pending, Fyrc already boasts an SSA (Static Single Assignment) based Intermediate Representation with basic SSA passes and SSA-based Register Allocation. A list of implemented and upcoming features can be found in the roadmap below.

## Roadmap
**Note:** The roadmap is not static and is subject to change. Also the sequence of items is tentative and features may not strictly be implemented in the given sequence.

- [x] Backend: SSA-based Intermediate Representation
- [x] Backend: Lazy direct AST to SSA translation algorithm
- [x] Backend: SSA passes
  - [x] Critical Edge Removal
  - [x] DFS Tree
  - [x] Loop Nesting Forest
  - [x] Dominator Tree
  - [x] Liveness Analysis
  - [x] Interference Graph
  - [x] Transformed SSA to Conventional SSA Translation
  - [x] Value Def/Use Points
  - [x] Value Global Next Use
- [x] Backend: SSA based Register Allocation
  - [x] Register Spilling and Live Range Splitting
  - [x] Register and Spill Slot Assignment
- [ ] Backend: Thumb Machine Code Generation
  - [x] Machine Instruction (Machinst) IR
  - [ ] Machine Code Generation
  - [ ] Machine Code & Data Layout Generation
- [ ] Frontend: Basic Language Design with Parser
  - [ ] Handwritten Parser
  - [ ] Basic Type Checking and Inference
  - [ ] Lowering to SSA
  - [ ] Support for
    - [ ] Integers & Integer Pointers
    - [ ] Control Flow (Functions, Loops, if/else)
    - [ ] Static immutable data
- [ ] Testing and QoL Improvements:
  - [ ] End to End compiler tests
  - [ ] Fuzzing framework
  - [ ] Basic example programs/games
  - [ ] Web Playground (WASM)
  - [ ] Basic Language docs
  - [ ] Basic Compiler Design docs
- [ ] Frontend+Backend: Aggregate types support (structs & arrays)
  - [ ] Heap allocated
  - [ ] Stack allocated
- [ ] Backend: More SSA passes
  - [ ] Scalar Replacement of Aggregates
  - [ ] Control Flow optimization passes
    - [ ] ...
  - [ ] Data Flow optimization passes
    - [ ] ...
- [ ] Backend: ARM Machine Code Generation
- [ ] Backend: ARM and Thumb Interop
- [ ] Frontend+Backend: Link Region support for functions and globals (ROM/IWRAM/EWRAM)
- [ ] Frontend+Backend: Support for Static Mutable data.
- [ ] Machine Code optimization framework using E-Graphs & Equality Saturation