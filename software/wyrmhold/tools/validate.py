#!/usr/bin/env python3
"""Static source validation for the canonical Wyrmhold build."""

from __future__ import annotations

import ast
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
ERRORS: list[str] = []
WARNINGS: list[str] = []
PASSES: list[str] = []


def error(message: str) -> None:
    ERRORS.append(message)


def warn(message: str) -> None:
    WARNINGS.append(message)


def passed(message: str) -> None:
    PASSES.append(message)


def source(name: str) -> str:
    return (ROOT / name).read_text(encoding="ascii")


class SafeEval(ast.NodeVisitor):
    OPS = {
        ast.Add: lambda a, b: a + b,
        ast.Sub: lambda a, b: a - b,
        ast.Mult: lambda a, b: a * b,
        ast.Div: lambda a, b: a // b,
        ast.FloorDiv: lambda a, b: a // b,
        ast.LShift: lambda a, b: a << b,
        ast.RShift: lambda a, b: a >> b,
        ast.BitOr: lambda a, b: a | b,
        ast.BitAnd: lambda a, b: a & b,
    }

    def __init__(self, names: dict[str, int]):
        self.names = names

    def visit_Expression(self, node: ast.Expression) -> int:
        return self.visit(node.body)

    def visit_Constant(self, node: ast.Constant) -> int:
        if not isinstance(node.value, int):
            raise ValueError
        return node.value

    def visit_Name(self, node: ast.Name) -> int:
        return self.names[node.id]

    def visit_BinOp(self, node: ast.BinOp) -> int:
        op = self.OPS.get(type(node.op))
        if op is None:
            raise ValueError
        return op(self.visit(node.left), self.visit(node.right))

    def visit_UnaryOp(self, node: ast.UnaryOp) -> int:
        if isinstance(node.op, ast.USub):
            return -self.visit(node.operand)
        if isinstance(node.op, ast.UAdd):
            return self.visit(node.operand)
        raise ValueError

    def generic_visit(self, node: ast.AST) -> int:
        raise ValueError(f"unsupported expression node: {type(node).__name__}")


def normalize_expr(expr: str) -> str:
    expr = re.sub(r"\$([0-9A-Fa-f]+)", r"0x\1", expr)
    expr = re.sub(r"%([01]+)", r"0b\1", expr)
    return expr.replace("/", "//")


def evaluate(expr: str, names: dict[str, int]) -> int:
    tree = ast.parse(normalize_expr(expr.strip()), mode="eval")
    return SafeEval(names).visit(tree)


def parse_constants() -> dict[str, int]:
    pending: dict[str, str] = {}
    for line in source("defines.asm").splitlines():
        code = line.split(";", 1)[0].strip()
        match = re.match(r"^([A-Z][A-Z0-9_]*)\s*=\s*(.+)$", code)
        if match and ".DEFINE" not in code:
            pending[match.group(1)] = match.group(2).strip()

    names: dict[str, int] = {}
    while pending:
        progress = False
        for name, expr in list(pending.items()):
            try:
                names[name] = evaluate(expr, names)
            except (KeyError, ValueError, SyntaxError):
                continue
            del pending[name]
            progress = True
        if not progress:
            break
    return names


CONSTANTS = parse_constants()


def byte_items(line: str) -> list[str]:
    code = line.split(";", 1)[0]
    match = re.search(r"\.BYTE\s+(.+)$", code, re.IGNORECASE)
    if not match:
        return []
    items = []
    current = []
    depth = 0
    quoted = False
    for char in match.group(1):
        if char == '"':
            quoted = not quoted
        elif not quoted and char == "(":
            depth += 1
        elif not quoted and char == ")":
            depth -= 1
        if char == "," and not quoted and depth == 0:
            items.append("".join(current).strip())
            current = []
        else:
            current.append(char)
    items.append("".join(current).strip())
    return items


def label_block(text: str, label: str) -> list[str]:
    match = re.search(
        rf"(?m)^{re.escape(label)}:\s*$([\s\S]*?)(?=^\S[^;\r\n]*:\s*$|\Z)",
        text,
    )
    if not match:
        error(f"missing label {label}")
        return []
    return match.group(1).splitlines()


def map_rows(text: str, label: str, count: int) -> list[str]:
    lines = text.splitlines()
    start = next((i for i, line in enumerate(lines) if line.strip() == f"{label}:"), None)
    if start is None:
        error(f"missing map {label}")
        return []
    rows = []
    for line in lines[start + 1 :]:
        match = re.search(r'\.BYTE\s+"([^"]*)"\s*,\s*0', line)
        if match:
            rows.append(match.group(1))
            if len(rows) == count:
                break
    return rows


def validate_maps() -> None:
    text = source("world.asm")
    specs = [
        ("ow_src", CONSTANTS["OWW"], CONSTANTS["OWH"], False),
        ("eastmere_src", CONSTANTS["TOWNW"], CONSTANTS["TOWNH"], True),
        ("valehaven_src", CONSTANTS["TOWNW"], CONSTANTS["TOWNH"], True),
        ("dung_src", CONSTANTS["DUNGW"], CONSTANTS["DUNGH"], True),
        ("castle_src", CONSTANTS["CASTLEW"], CONSTANTS["CASTLEH"], True),
        ("shrine_src", CONSTANTS["SHRINEW"], CONSTANTS["SHRINEH"], True),
    ]
    allowed = set(".&^~TOC:=#+$<Sh% ")
    for label, width, height, strict_width in specs:
        rows = map_rows(text, label, height)
        if len(rows) != height:
            error(f"{label}: expected {height} rows, found {len(rows)}")
            continue
        bad_chars = sorted(set("".join(rows)) - allowed)
        if bad_chars:
            error(f"{label}: unsupported map characters {bad_chars}")
        bad_widths = [(index, len(row)) for index, row in enumerate(rows) if len(row) != width]
        if bad_widths and strict_width:
            error(f"{label}: rows with non-{width} widths: {bad_widths}")
        elif bad_widths:
            warn(f"{label}: {len(bad_widths)} rows rely on decoder padding/truncation")
        else:
            passed(f"{label}: {width}x{height}")

    rows = map_rows(text, "ow_src", CONSTANTS["OWH"])
    discoveries = [
        ("cache", "CACHE_X", "CACHE_Y", "&"),
        ("cairn", "CAIRN_X", "CAIRN_Y", "h"),
        ("waystone", "WAYSTONE_X", "WAYSTONE_Y", ":"),
        ("reed ford west", "SHORTCUT_A_X", "SHORTCUT_A_Y", "%"),
        ("reed ford east", "SHORTCUT_B_X", "SHORTCUT_B_Y", "%"),
    ]
    for name, x_name, y_name, expected in discoveries:
        x, y = CONSTANTS[x_name], CONSTANTS[y_name]
        decoded = rows[y][: CONSTANTS["OWW"]].ljust(CONSTANTS["OWW"], ".")
        if decoded[x] != expected:
            error(f"{name}: expected {expected!r} at ({x},{y}), found {decoded[x]!r}")
    passed("discovery coordinates match authored terrain")


def validate_overworld_shortcut() -> None:
    player = source("player.asm")
    main = source("wyrmhold.asm")

    if re.search(r"(?m)^shortcut_cross:\s*$", player):
        passed("reed-ford shortcut routine present")
    else:
        error("missing reed-ford shortcut routine")

    for name in ("SHORTCUT_HP_COST", "SHORTCUT_FOOD_COST"):
        if name in CONSTANTS:
            passed(f"reed-ford constant present: {name}")
        else:
            error(f"missing reed-ford constant: {name}")

    if re.search(r"\bJSR\s+use_action\b[\s\S]*?\bJSR\s+mon_act\b", main):
        passed("contextual shortcut can consume a monster turn")
    else:
        error("contextual use actions cannot consume a monster turn")

    if "m_shortcut_cross" in source("combat.asm"):
        passed("reed-ford feedback message present")
    else:
        error("missing reed-ford feedback message")


def validate_tile_tables() -> None:
    text = source("world.asm")
    expected = CONSTANTS["NUM_TILES"]
    for label in ("tile_color", "tile_variant", "tile_prop"):
        count = sum(len(byte_items(line)) for line in label_block(text, label))
        if count != expected:
            error(f"{label}: expected {expected} entries, found {count}")
        else:
            passed(f"{label}: {count} entries")

    chars = re.search(r'dec_chars:\s*\r?\n\s*\.BYTE\s+"([^"]*)"', text)
    codes = sum(len(byte_items(line)) for line in label_block(text, "dec_codes"))
    if chars and len(chars.group(1)) != codes:
        error(f"decoder tables differ: {len(chars.group(1))} chars, {codes} codes")
    else:
        passed(f"decoder tables: {codes} entries")


def validate_gameplay_tables() -> None:
    entity = source("entity.asm")
    for label in ("mtype_glyph", "mtype_color", "mtype_hp", "mtype_atk", "mtype_xp", "mtype_gold"):
        count = sum(len(byte_items(line)) for line in label_block(entity, label))
        expected = CONSTANTS["NUM_MTYPE"]
        if count != expected:
            error(f"{label}: expected {expected} entries, found {count}")
        else:
            passed(f"{label}: {count} entries")

    count = sum(len(byte_items(line)) for line in label_block(entity, "region_encounters"))
    expected = CONSTANTS["NUM_REGIONS"] * CONSTANTS["REGION_ENC_SIZE"] * 2
    if count != expected:
        error(f"region_encounters: expected {expected} entries, found {count}")
    else:
        passed(f"region_encounters: {count} entries")


def validate_rng_entropy() -> None:
    rng = source("rng.asm")
    main = source("wyrmhold.asm")

    for label in ("rng_timing_tick", "rng_mix_timing"):
        if re.search(rf"(?m)^{label}:\s*$", rng):
            passed(f"RNG entropy routine present: {label}")
        else:
            error(f"missing RNG entropy routine: {label}")

    timing_calls = len(re.findall(r"\bJSR\s+rng_timing_tick\b", main))
    if timing_calls < 2:
        error(f"title screen needs at least two rng_timing_tick calls, found {timing_calls}")
    else:
        passed(f"title screen timing entropy calls: {timing_calls}")

    mix_calls = len(re.findall(r"\bJSR\s+rng_mix_timing\b", main))
    if mix_calls != 1:
        error(f"title screen needs exactly one rng_mix_timing call, found {mix_calls}")
    else:
        passed("title screen mixes timing entropy before new game")

    if re.search(r"\bORA\s+seedhi\b", rng):
        passed("RNG entropy mix guards against all-zero state")
    else:
        error("RNG entropy mix lacks an all-zero state guard")


def validate_help_panel() -> None:
    main = source("wyrmhold.asm")
    ui = source("ui.asm")

    if re.search(r"(?m)^help_menu:\s*$", ui):
        passed("in-game help panel routine present")
    else:
        error("missing in-game help panel routine")

    if re.search(r"\bCMP\s+#'\?'", main) and re.search(r"\bJSR\s+help_menu\b", main):
        passed("question-mark key opens in-game help")
    else:
        error("question-mark key is not wired to in-game help")

    if re.search(r"help_draw:[\s\S]*?\bJSR\s+name_objective\b", ui):
        passed("in-game help displays current objective")
    else:
        error("in-game help does not display current objective")

    strings = re.findall(
        r'(?m)^help_[a-z0-9_]+:\s*\r?\n\s*\.BYTE\s+"([^"]*)",0',
        ui,
    )
    too_wide = [text for text in strings if len(text) > 38]
    if too_wide:
        error(f"in-game help strings exceed panel width: {too_wide}")
    else:
        passed(f"in-game help strings fit panel: {len(strings)} lines")

def glyph_records() -> list[tuple[int, str, int]]:
    records: list[tuple[int, str, int]] = []
    for filename in ("tiles.asm", "metatiles.asm"):
        for line_no, line in enumerate(source(filename).splitlines(), 1):
            items = byte_items(line)
            if not items:
                continue
            try:
                code = evaluate(items[0], CONSTANTS)
            except (KeyError, ValueError, SyntaxError):
                continue
            if filename == "metatiles.asm":
                if len(items) != 9:
                    error(f"{filename}:{line_no}: metatile glyph must have code plus 8 rows")
                    continue
            elif len(items) == 1 and re.match(r"^G_[A-Z0-9_]+$", items[0]):
                # Title glyph code; its eight rows follow on separate lines.
                pass
            else:
                continue
            records.append((code, filename, line_no))
    return records


def validate_glyphs() -> None:
    records = glyph_records()
    owners: dict[int, list[str]] = {}
    for code, filename, line_no in records:
        owners.setdefault(code, []).append(f"{filename}:{line_no}")
        if not 1 <= code <= 255:
            error(f"{filename}:{line_no}: glyph code {code} outside 1..255")
    duplicates = {code: refs for code, refs in owners.items() if len(refs) > 1}
    if duplicates:
        for code, refs in duplicates.items():
            error(f"glyph ${code:02X} allocated more than once: {', '.join(refs)}")
    else:
        passed(f"glyph allocation: {len(owners)} unique custom glyphs")

    free_high = [code for code in range(0xF8, 0x100) if code not in owners]
    passed(f"free high glyphs: {', '.join(f'${code:02X}' for code in free_high)}")

    text = source("metatiles.asm")
    groups: dict[str, set[int]] = {}
    for line_no, line in enumerate(text.splitlines(), 1):
        items = byte_items(line)
        if len(items) != 9:
            continue
        match = re.match(r"([A-Z][A-Z0-9_]*)\+([0-3])$", items[0])
        if not match:
            error(f"metatiles.asm:{line_no}: expected BASE+quadrant glyph expression")
            continue
        groups.setdefault(match.group(1), set()).add(int(match.group(2)))
    incomplete = {name: sorted(quads) for name, quads in groups.items() if quads != {0, 1, 2, 3}}
    if incomplete:
        for name, quads in incomplete.items():
            error(f"{name}: incomplete metatile quadrants {quads}")
    else:
        passed(f"metatile groups: {len(groups)} complete 2x2 records")

    signatures: dict[tuple[str, ...], list[str]] = {}
    for line_no, line in enumerate(text.splitlines(), 1):
        items = byte_items(line)
        if len(items) == 9:
            signatures.setdefault(tuple(items[1:]), []).append(f"line {line_no}")
    duplicate_art = [refs for refs in signatures.values() if len(refs) > 1]
    if duplicate_art:
        warn(f"metatiles.asm: {len(duplicate_art)} duplicated glyph bitmap patterns")
    else:
        passed("metatile glyph bitmap patterns are unique")

    # Known artwork-label mismatch remains visible until an art audit decides it.
    if re.search(r"; town:.*?\n\s*\.BYTE\s+MG_CASTLE\+0", text, re.DOTALL):
        warn("metatiles.asm: town artwork uses MG_CASTLE labels")
    if re.search(r"; castle:.*?\n\s*\.BYTE\s+MG_TOWN\+0", text, re.DOTALL):
        warn("metatiles.asm: castle artwork uses MG_TOWN labels")

    tile_lines = source("tiles.asm").splitlines()
    title_count = 0
    for index, line in enumerate(tile_lines):
        items = byte_items(line)
        if len(items) != 1 or not re.match(r"^G_[A-Z0-9_]+$", items[0]):
            continue
        title_count += 1
        rows = [byte_items(row) for row in tile_lines[index + 1 : index + 9]]
        if len(rows) != 8 or any(len(row) != 1 for row in rows):
            error(f"tiles.asm:{index + 1}: {items[0]} must be followed by eight scanlines")
    passed(f"title glyph records: {title_count}")


def validate_link_map() -> None:
    path = ROOT / "wyrmhold.map"
    if not path.exists():
        warn("wyrmhold.map absent; linked end-address check skipped")
        return
    text = path.read_text(encoding="ascii")
    match = re.search(r"(?m)^TEA\s+[0-9A-F]+\s+([0-9A-F]+)\s+[0-9A-F]+", text)
    if not match:
        error("wyrmhold.map: cannot find TEA segment end address")
        return
    end = int(match.group(1), 16)
    if end >= 0x9000:
        error(f"linked TEA end ${end:04X} exceeds target below $9000")
    else:
        passed(f"linked TEA end ${end:04X} is below $9000")


def main() -> int:
    validate_maps()
    validate_overworld_shortcut()
    validate_tile_tables()
    validate_gameplay_tables()
    validate_rng_entropy()
    validate_help_panel()
    validate_glyphs()
    validate_link_map()

    for message in PASSES:
        print(f"PASS: {message}")
    for message in WARNINGS:
        print(f"WARN: {message}")
    for message in ERRORS:
        print(f"ERROR: {message}")
    print(f"\n{len(PASSES)} passed, {len(WARNINGS)} warnings, {len(ERRORS)} errors")
    return 1 if ERRORS else 0


if __name__ == "__main__":
    sys.exit(main())
