## json2dir: create the directory tree a JSON document describes, in the current directory.
## Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
##
## Pure Roc on the official basic-cli platform. basic-cli has no way to create a
## symbolic link or to set execute permissions, so per RFC section 10 documents
## containing ["link", ...] or ["script", ...] are rejected instead of approximated.
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst" }

import pf.OsStr
import pf.Path
import pf.Stdin
import pf.Stderr

## A parsed document value. Names and contents are kept as raw UTF-8 bytes.
Value := [Obj(List((List(U8), Value))), Text(List(U8)), Link(List(U8)), Script(List(U8))]

main! : List(OsStr) => Try({}, _)
main! = |args| {
	if !args.is_empty() {
		Stderr.line!("usage: json2dir < document.json") ?? {}
		return Err(Exit(2))
	}

	match run!() {
		Ok({}) => Ok({})
		Err(msg) => {
			Stderr.line!("json2dir: ${msg}") ?? {}
			Err(Exit(1))
		}
	}
}

run! : () => Try({}, Str)
run! = || {
	input = Stdin.read_to_end!() ? |_| "cannot read standard input"
	root = parse_document(input)?
	match root {
		Obj(members) => apply!([], members)
		_ => Err("the root of the document must be an object")
	}
}

# ---------------------------------------------------------------------------
# Section 3: strict UTF-8 and strict JSON

byte_at : List(U8), U64 -> U8
byte_at = |s, i| s.get(i) ?? 0

## Strict UTF-8 (RFC 3629): no overlong forms, no surrogates, nothing above U+10FFFF.
valid_utf8 : List(U8) -> Bool
valid_utf8 = |s| {
	len = s.len()
	var $i = 0
	while $i < len {
		b = byte_at(s, $i)
		if b < 0x80 {
			$i = $i + 1
		} else {
			# (number of continuation bytes, allowed range of the first one)
			(n, lo, hi) =
				if b >= 0xC2 and b <= 0xDF {
					(1, 0x80, 0xBF)
				} else if b == 0xE0 {
					(2, 0xA0, 0xBF)
				} else if b == 0xED {
					(2, 0x80, 0x9F)
				} else if b >= 0xE1 and b <= 0xEF {
					(2, 0x80, 0xBF)
				} else if b == 0xF0 {
					(3, 0x90, 0xBF)
				} else if b >= 0xF1 and b <= 0xF3 {
					(3, 0x80, 0xBF)
				} else if b == 0xF4 {
					(3, 0x80, 0x8F)
				} else {
					(0, 0, 0)
				}
			if n == 0 or $i + n >= len {
				return False
			}
			b1 = byte_at(s, $i + 1)
			if b1 < lo or b1 > hi {
				return False
			}
			var $k = 2
			while $k <= n {
				c = byte_at(s, $i + $k)
				if c < 0x80 or c > 0xBF {
					return False
				}
				$k = $k + 1
			}
			$i = $i + n + 1
		}
	}
	True
}

is_ws : U8 -> Bool
is_ws = |c| c == ' ' or c == '\t' or c == '\n' or c == '\r'

skip_ws : List(U8), U64 -> U64
skip_ws = |s, p| {
	var $p = p
	while is_ws(byte_at(s, $p)) {
		$p = $p + 1
	}
	$p
}

parse_document : List(U8) -> Try(Value, Str)
parse_document = |input| {
	if !valid_utf8(input) {
		return Err("input is not valid UTF-8")
	}
	# A leading byte order mark is ignored, as section 3 allows.
	start = if input.starts_with([0xEF, 0xBB, 0xBF]) { 3 } else { 0 }
	p = skip_ws(input, start)
	if p >= input.len() {
		return Err("input is not valid JSON: empty document")
	}
	parsed = parse_value(input, p, 0)?
	end = skip_ws(input, parsed.pos)
	if end != input.len() {
		return Err("input is not valid JSON: trailing data at byte ${end.to_str()}")
	}
	Ok(parsed.value)
}

parse_value : List(U8), U64, U64 -> Try({ value : Value, pos : U64 }, Str)
parse_value = |s, p, depth| {
	if depth > 10000 {
		return Err("document is nested too deeply")
	}
	c = byte_at(s, p)
	if p >= s.len() {
		Err("input is not valid JSON: unexpected end of input")
	} else if c == '{' {
		parse_object(s, p, depth)
	} else if c == '[' {
		parse_array(s, p, depth)
	} else if c == '"' {
		str = parse_string(s, p)?
		Ok({ value: Text(str.bytes), pos: str.pos })
	} else if c == '-' or (c >= '0' and c <= '9') or c == 't' or c == 'f' or c == 'n' {
		Err("numbers, true, false and null are not allowed (at byte ${p.to_str()})")
	} else {
		Err("input is not valid JSON: unexpected byte at ${p.to_str()}")
	}
}

parse_object : List(U8), U64, U64 -> Try({ value : Value, pos : U64 }, Str)
parse_object = |s, start, depth| {
	var $p = skip_ws(s, start + 1)
	var $members = []
	if byte_at(s, $p) == '}' {
		return Ok({ value: Obj([]), pos: $p + 1 })
	}
	while True {
		if byte_at(s, $p) != '"' {
			return Err("input is not valid JSON: expected a member name at byte ${$p.to_str()}")
		}
		name = parse_string(s, $p)?
		$p = skip_ws(s, name.pos)
		if byte_at(s, $p) != ':' {
			return Err("input is not valid JSON: expected ':' at byte ${$p.to_str()}")
		}
		$p = skip_ws(s, $p + 1)
		member = parse_value(s, $p, depth + 1)?
		check_name(name.bytes)?
		# Section 4.2: duplicate names are rejected.
		if $members.any(|(n, _)| n == name.bytes) {
			return Err("duplicate name ${Str.from_utf8_lossy(name.bytes)}")
		}
		$members = $members.append((name.bytes, member.value))
		$p = skip_ws(s, member.pos)
		c = byte_at(s, $p)
		if c == ',' {
			$p = skip_ws(s, $p + 1)
		} else if c == '}' {
			return Ok({ value: Obj($members), pos: $p + 1 })
		} else {
			return Err("input is not valid JSON: expected ',' or '}' at byte ${$p.to_str()}")
		}
	}
	Err("unreachable")
}

parse_array : List(U8), U64, U64 -> Try({ value : Value, pos : U64 }, Str)
parse_array = |s, start, depth| {
	var $p = skip_ws(s, start + 1)
	var $items = []
	if byte_at(s, $p) != ']' {
		var $more = True
		while $more {
			item = parse_value(s, $p, depth + 1)?
			$items = $items.append(item.value)
			$p = skip_ws(s, item.pos)
			c = byte_at(s, $p)
			if c == ',' {
				$p = skip_ws(s, $p + 1)
			} else if c == ']' {
				$more = False
			} else {
				return Err("input is not valid JSON: expected ',' or ']' at byte ${$p.to_str()}")
			}
		}
	}
	end = $p + 1
	# Section 4.4: exactly two strings, kind "link" or "script".
	match $items {
		[Text(kind), Text(payload)] =>
			if kind == Str.to_utf8("link") {
				Ok({ value: Link(payload), pos: end })
			} else if kind == Str.to_utf8("script") {
				Ok({ value: Script(payload), pos: end })
			} else {
				Err("unknown array kind ${Str.from_utf8_lossy(kind)}")
			}
		_ => Err("an array must be [\"link\", target] or [\"script\", content]")
	}
}

hex_value : U8 -> Try(U32, Str)
hex_value = |c|
	if c >= '0' and c <= '9' {
		Ok(U8.to_u32(c - '0'))
	} else if c >= 'a' and c <= 'f' {
		Ok(U8.to_u32(c - 'a') + 10)
	} else if c >= 'A' and c <= 'F' {
		Ok(U8.to_u32(c - 'A') + 10)
	} else {
		Err("input is not valid JSON: bad \\u escape")
	}

read_hex4 : List(U8), U64 -> Try(U32, Str)
read_hex4 = |s, p| {
	a = hex_value(byte_at(s, p))?
	b = hex_value(byte_at(s, p + 1))?
	c = hex_value(byte_at(s, p + 2))?
	d = hex_value(byte_at(s, p + 3))?
	Ok(a * 4096 + b * 256 + c * 16 + d)
}

## Append the UTF-8 encoding of a Unicode scalar value.
push_utf8 : List(U8), U32 -> List(U8)
push_utf8 = |out, cp|
	if cp < 0x80 {
		out.append(U32.to_u8_wrap(cp))
	} else if cp < 0x800 {
		out
			.append(U32.to_u8_wrap(0xC0 + U32.shr_zf_wrap(cp, 6)))
			.append(U32.to_u8_wrap(0x80 + cp.bitwise_and(0x3F)))
	} else if cp < 0x10000 {
		out
			.append(U32.to_u8_wrap(0xE0 + U32.shr_zf_wrap(cp, 12)))
			.append(U32.to_u8_wrap(0x80 + U32.shr_zf_wrap(cp, 6).bitwise_and(0x3F)))
			.append(U32.to_u8_wrap(0x80 + cp.bitwise_and(0x3F)))
	} else {
		out
			.append(U32.to_u8_wrap(0xF0 + U32.shr_zf_wrap(cp, 18)))
			.append(U32.to_u8_wrap(0x80 + U32.shr_zf_wrap(cp, 12).bitwise_and(0x3F)))
			.append(U32.to_u8_wrap(0x80 + U32.shr_zf_wrap(cp, 6).bitwise_and(0x3F)))
			.append(U32.to_u8_wrap(0x80 + cp.bitwise_and(0x3F)))
	}

## Parse a JSON string starting at the opening quote; returns its decoded UTF-8 bytes.
## The input is already known to be valid UTF-8, so raw bytes are copied as they are.
parse_string : List(U8), U64 -> Try({ bytes : List(U8), pos : U64 }, Str)
parse_string = |s, start| {
	len = s.len()
	var $p = start + 1
	var $out = []
	while True {
		if $p >= len {
			return Err("input is not valid JSON: unterminated string")
		}
		c = byte_at(s, $p)
		if c == '"' {
			return Ok({ bytes: $out, pos: $p + 1 })
		} else if c < 0x20 {
			return Err("input is not valid JSON: control character in string at byte ${$p.to_str()}")
		} else if c == '\\' {
			e = byte_at(s, $p + 1)
			if e == '"' or e == '\\' or e == '/' {
				$out = $out.append(e)
				$p = $p + 2
			} else if e == 'b' {
				$out = $out.append(8)
				$p = $p + 2
			} else if e == 'f' {
				$out = $out.append(12)
				$p = $p + 2
			} else if e == 'n' {
				$out = $out.append(10)
				$p = $p + 2
			} else if e == 'r' {
				$out = $out.append(13)
				$p = $p + 2
			} else if e == 't' {
				$out = $out.append(9)
				$p = $p + 2
			} else if e == 'u' {
				u = read_hex4(s, $p + 2)?
				if u >= 0xDC00 and u <= 0xDFFF {
					return Err("string contains an unpaired surrogate")
				} else if u >= 0xD800 and u <= 0xDBFF {
					# A high surrogate must be followed by an escaped low surrogate.
					if byte_at(s, $p + 6) != '\\' or byte_at(s, $p + 7) != 'u' {
						return Err("string contains an unpaired surrogate")
					}
					low = read_hex4(s, $p + 8)?
					if low < 0xDC00 or low > 0xDFFF {
						return Err("string contains an unpaired surrogate")
					}
					cp = 0x10000 + (u - 0xD800) * 0x400 + (low - 0xDC00)
					$out = push_utf8($out, cp)
					$p = $p + 12
				} else {
					$out = push_utf8($out, u)
					$p = $p + 6
				}
			} else {
				return Err("input is not valid JSON: bad escape at byte ${$p.to_str()}")
			}
		} else {
			$out = $out.append(c)
			$p = $p + 1
		}
	}
	Err("unreachable")
}

# ---------------------------------------------------------------------------
# Section 4.2.1: names are used exactly; "a/" style names are rejected, not trimmed.

check_name : List(U8) -> Try({}, Str)
check_name = |name|
	if name.is_empty() or name == ['.'] or name == ['.', '.'] or name.contains('/') or name.contains(0) {
		Err("invalid name ${Str.inspect(Str.from_utf8_lossy(name))}")
	} else {
		Ok({})
	}

# ---------------------------------------------------------------------------
# Sections 4 and 5: create the tree

join : List(U8), List(U8) -> List(U8)
join = |dir, name| if dir.is_empty() { name } else { dir.append('/').concat(name) }

show : List(U8) -> Str
show = |p| Str.from_utf8_lossy(p)

io_err : Str, List(U8) -> ([PathErr(_, _)] -> Str)
io_err = |what, p| |PathErr(err, _)| "${show(p)}: cannot ${what}: ${err.to_str()}"

## Section 5: what is at this path now, without following a symbolic link.
existing! : List(U8) => Try([Missing, Dir, NotDir], Str)
existing! = |p|
	match Path.unix_bytes(p).type!() {
		Ok(IsDir) => Ok(Dir)
		Ok(_) => Ok(NotDir)
		Err(PathErr(NotFound, _)) => Ok(Missing)
		Err(PathErr(err, _)) => Err("${show(p)}: cannot inspect: ${err.to_str()}")
	}

## Validate everything before touching the file system (section 6).
validate : List((List(U8), Value)), List(U8) -> Try({}, Str)
validate = |members, dir|
	members.fold_try({}, |{}, (name, value)| {
		p = join(dir, name)
		match value {
			Obj(children) => validate(children, p)
			Text(_) => Ok({})
			Link(_) => Err("${show(p)}: symbolic links are not supported (basic-cli cannot create them)")
			Script(_) => Err("${show(p)}: scripts are not supported (basic-cli cannot set execute permissions)")
		}
	})

apply! : List(U8), List((List(U8), Value)) => Try({}, Str)
apply! = |dir, members| {
	if dir.is_empty() {
		validate(members, dir)?
	}
	for (name, value) in members {
		p = join(dir, name)
		path = Path.unix_bytes(p)
		state = existing!(p)?
		match value {
			Obj(children) => {
				if state == NotDir {
					path.delete!() ? io_err("remove", p)
				}
				if state != Dir {
					path.create_dir!() ? io_err("create directory", p)
				}
				apply!(p, children)?
			}
			Text(content) => {
				if state == Dir {
					return Err("${show(p)}: a directory is in the way")
				}
				if state == NotDir {
					path.delete!() ? io_err("remove", p)
				}
				path.write_bytes!(content) ? io_err("write", p)
			}
			_ => return Err("${show(p)}: unsupported entry")
		}
	}
	Ok({})
}
