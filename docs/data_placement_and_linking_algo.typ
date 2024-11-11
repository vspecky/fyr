#import "@preview/showybox:2.0.1": showybox
#import "@preview/algorithmic:0.1.0"
#import algorithmic: algorithm

#set par(justify: true)

= Fyr Data/Code Placement and Linking
The purpose of this document is to provide a specification for the algorithm implemented by _Fyr_
for placing code and data and linking them together.

#let var(term) = $italic(term)$
#let arg(ty, v) = [#ty #var(v)]
#let i = math.italic
#let null = var("null")

#showybox([
  #algorithm({
    import algorithmic: *
    State[*type* Offset = Int]
    State[]
    Function("In-Range", args: (arg("Offset", "me"), arg("Offset", "data")), {
      Return[$#i("me") < #i("data") and (#i("me")slash 4 + 256 >= #i("data")slash 4)$]
    })

    State[]
    Procedure(
      "Place-Block-Data",
      args: (
        arg("DLL", "layout"),
        arg("Function", "func"),
        arg("Block", "block"),
        arg("Slot", "slot"),
      ),
      {
        let block = var("block")
        let slot = var("slot")

        let ld_instrs = var("ld_instrs")
        Assign(ld_instrs, FnI("list")[])
        let instr = var("instr")
        For(cond: [#instr *in* #var("block.instructions")], {
          If(cond: [#instr.#var("is_load")], {
            State[$#ld_instrs.#var("prepend")\(#instr)$]
          })
        })

        State[]
        let i = var("i")
        Assign(i, $#ld_instrs.#var("length") - 1$)

        While(cond: $#i >= 0$, {
          let offset = var("offset")
          let size = var("size")
          let branch = var("branch")

          Cmt([Calculate the offset of the first byte after the block])
          let block_end_offset = var("beoff")
          Assign(block_end_offset, $#slot.#offset + #slot.#size + #block.#branch.#size$)

          let data_offset = var("doff")
          If(cond: $#block_end_offset "mod" 4 > 0$, {
            Cmt[If the byte is not word aligned, we need to add a null padding slot]
            Cmt[after the function slot.]
            let padding = var("padding")
            Assign(padding, $#block_end_offset + (#block_end_offset "mod" 4 - 1)$)
            State[#slot.#var("add_null_padding_to_right")\(#padding)]
            Assign(slot, [#slot.#var("right")])
            Assign(data_offset, [#slot.#offset])
          })
          Else({
            Assign(data_offset, block_end_offset)
          })

          State[]
          let slot_map = var("slot_map")
          Assign(slot_map, ${}$)
          let longest_reach = var("longest_reach")
          Assign(longest_reach, null)

          While(cond: $#i >= 0$, {
            let val = var("val")
            Assign(instr, $#ld_instrs\[#i]$)
            let existing = var("existing")
            Assign(existing, $#slot_map\[#instr.#val]$)
            If(cond: [$#existing != #null and$ #CallI("In-Range")[#instr.#offset, #existing.#offset]])
          })
        })
      }
    )
    State[]
    Function("Fibonacci", args: ($n$,), {
      Assign($"data_adjacent_slot"$, $"next multiple of 4"$)
      If(cond: $n <= 1$, {
        Return[$1$]
      })
      Else({
        Return[#CallI("Fibonacci")[$n - 1$] $+$ #CallI("Fibonacci")[$n - 2$]]
      })
    })
  })
])