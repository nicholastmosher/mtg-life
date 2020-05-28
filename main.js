(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.a4.ak === region.bj.ak)
	{
		return 'on line ' + region.a4.ak;
	}
	return 'on lines ' + region.a4.ak + ' through ' + region.bj.ak;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList === 'function' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cV,
		impl.dM,
		impl.dv,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		F: func(record.F),
		a5: record.a5,
		a0: record.a0
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.F;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.a5;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.a0) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cV,
		impl.dM,
		impl.dv,
		function(sendToApp, initialModel) {
			var view = impl.dN;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cV,
		impl.dM,
		impl.dv,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.a3 && impl.a3(sendToApp)
			var view = impl.dN;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.cq);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.dK) && (_VirtualDom_doc.title = title = doc.dK);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.c6;
	var onUrlRequest = impl.c7;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		a3: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bP === next.bP
							&& curr.br === next.br
							&& curr.bM.a === next.bM.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		cV: function(flags)
		{
			return A3(impl.cV, flags, _Browser_getUrl(), key);
		},
		dN: impl.dN,
		dM: impl.dM,
		dv: impl.dv
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cP: 'hidden', cw: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cP: 'mozHidden', cw: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cP: 'msHidden', cw: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cP: 'webkitHidden', cw: 'webkitvisibilitychange' }
		: { cP: 'hidden', cw: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		de: _Browser_getScene(),
		b3: {
			aM: _Browser_window.pageXOffset,
			aN: _Browser_window.pageYOffset,
			dO: _Browser_doc.documentElement.clientWidth,
			Z: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		dO: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		Z: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			de: {
				dO: node.scrollWidth,
				Z: node.scrollHeight
			},
			b3: {
				aM: node.scrollLeft,
				aN: node.scrollTop,
				dO: node.clientWidth,
				Z: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			de: _Browser_getScene(),
			b3: {
				aM: x,
				aN: y,
				dO: _Browser_doc.documentElement.clientWidth,
				Z: _Browser_doc.documentElement.clientHeight
			},
			cH: {
				aM: x + rect.left,
				aN: y + rect.top,
				dO: rect.width,
				Z: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.d) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.f),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.f);
		} else {
			var treeLen = builder.d * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.g) : builder.g;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.d);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.f) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.f);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{g: nodeList, d: (len / $elm$core$Array$branchFactor) | 0, f: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {bn: fragment, br: host, bK: path, bM: port_, bP: protocol, bQ: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$Health = 0;
var $author$project$Log$Log = $elm$core$Basics$identity;
var $author$project$Log$createLog = function (initial) {
	return {aX: initial, ar: _List_Nil};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $author$project$Main$createPlayer = function (_v0) {
	var id = _v0.a;
	var name = _v0.b;
	return {
		az: '',
		bi: true,
		P: $author$project$Log$createLog(40),
		aG: id,
		D: $elm$core$Dict$fromList(
			_List_fromArray(
				[
					_Utils_Tuple2(
					id,
					$author$project$Log$createLog(0))
				])),
		bA: name,
		U: $author$project$Log$createLog(0),
		al: id
	};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		{
			C: 0,
			bC: '',
			G: 1,
			c: $elm$core$Dict$fromList(
				_List_fromArray(
					[
						_Utils_Tuple2(
						0,
						$author$project$Main$createPlayer(
							_Utils_Tuple2(0, 'Player 0')))
					])),
			o: _List_fromArray(
				[0]),
			j: 0
		},
		$elm$core$Platform$Cmd$none);
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$ArrowDown = {$: 1};
var $author$project$Main$ArrowLeft = {$: 2};
var $author$project$Main$ArrowRight = {$: 3};
var $author$project$Main$ArrowUp = {$: 0};
var $author$project$Main$KeyPress = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$Other = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$toKey = function (string) {
	return $author$project$Main$KeyPress(
		function () {
			switch (string) {
				case 'ArrowUp':
					return $author$project$Main$ArrowUp;
				case 'ArrowDown':
					return $author$project$Main$ArrowDown;
				case 'ArrowLeft':
					return $author$project$Main$ArrowLeft;
				case 'ArrowRight':
					return $author$project$Main$ArrowRight;
				default:
					var other = string;
					return $author$project$Main$Other(other);
			}
		}());
};
var $author$project$Main$keyDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Main$toKey,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $elm$browser$Browser$Events$Document = 0;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {bL: pids, bY: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {bl: event, E: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.bL,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.E;
		var event = _v0.bl;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.bY);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, 0, 'keydown');
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$browser$Browser$Events$onKeyDown($author$project$Main$keyDecoder);
};
var $author$project$Main$Noop = {$: 0};
var $author$project$Main$UpdateHealth = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $author$project$Main$UpdatePoison = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$Main$addPlayer = F2(
	function (name, model) {
		var players = _Utils_ap(
			model.o,
			_List_fromArray(
				[model.G]));
		var playerStub = $author$project$Main$createPlayer(
			_Utils_Tuple2(model.G, name));
		var initializedPlayer = _Utils_update(
			playerStub,
			{
				D: $elm$core$Dict$fromList(
					A2(
						$elm$core$List$map,
						function (id) {
							return _Utils_Tuple2(
								id,
								$author$project$Log$createLog(0));
						},
						players))
			});
		var appendPlayerToPlayerInfo = F2(
			function (pid, pInfo) {
				return _Utils_update(
					pInfo,
					{
						D: A3(
							$elm$core$Dict$insert,
							pid,
							$author$project$Log$createLog(0),
							pInfo.D)
					});
			});
		var updatedPlayerInfos = A2(
			$elm$core$Dict$map,
			function (_v0) {
				return appendPlayerToPlayerInfo(playerStub.aG);
			},
			model.c);
		var playerInfo = A3($elm$core$Dict$insert, initializedPlayer.aG, initializedPlayer, updatedPlayerInfos);
		return _Utils_update(
			model,
			{G: model.G + 1, c: playerInfo, o: players});
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			A2(
				$elm$core$Task$onError,
				A2(
					$elm$core$Basics$composeL,
					A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
					$elm$core$Result$Err),
				A2(
					$elm$core$Task$andThen,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Ok),
					task)));
	});
var $elm$browser$Browser$Dom$focus = _Browser_call('focus');
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$browser$Browser$Dom$getViewportOf = _Browser_getViewportOf;
var $elm$browser$Browser$Dom$setViewportOf = _Browser_setViewportOf;
var $author$project$Main$scrollLog = function (id) {
	return A2(
		$elm$core$Task$attempt,
		function (_v0) {
			return $author$project$Main$Noop;
		},
		A2(
			$elm$core$Task$andThen,
			function (info) {
				return A3($elm$browser$Browser$Dom$setViewportOf, id, info.de.dO, 0);
			},
			$elm$browser$Browser$Dom$getViewportOf(id)));
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Log$current = function (_v0) {
	var initial = _v0.aX;
	var updates = _v0.ar;
	var _v1 = $elm$core$List$head(updates);
	if (_v1.$ === 1) {
		return initial;
	} else {
		var _v2 = _v1.a;
		var now = _v2.b;
		return now;
	}
};
var $author$project$Log$update = F2(
	function (diff, _v0) {
		var log = _v0;
		return _Utils_update(
			log,
			{
				ar: A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						diff,
						$author$project$Log$current(log) + diff),
					log.ar)
			});
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 0:
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				case 2:
					var key = msg.a;
					var _v1 = _Utils_Tuple2(key, model.C);
					switch (_v1.a.$) {
						case 0:
							if (_v1.b === 1) {
								var _v2 = _v1.a;
								var _v3 = _v1.b;
								var $temp$msg = A2($author$project$Main$UpdatePoison, model.j, 1),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							} else {
								var _v10 = _v1.a;
								var $temp$msg = A2($author$project$Main$UpdateHealth, model.j, 1),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							}
						case 1:
							if (_v1.b === 1) {
								var _v4 = _v1.a;
								var _v5 = _v1.b;
								var $temp$msg = A2($author$project$Main$UpdatePoison, model.j, -1),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							} else {
								var _v11 = _v1.a;
								var $temp$msg = A2($author$project$Main$UpdateHealth, model.j, -1),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							}
						case 2:
							if (_v1.b === 1) {
								var _v6 = _v1.a;
								var _v7 = _v1.b;
								var $temp$msg = A2($author$project$Main$UpdatePoison, model.j, -5),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							} else {
								var _v12 = _v1.a;
								var $temp$msg = A2($author$project$Main$UpdateHealth, model.j, -5),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							}
						case 3:
							if (_v1.b === 1) {
								var _v8 = _v1.a;
								var _v9 = _v1.b;
								var $temp$msg = A2($author$project$Main$UpdatePoison, model.j, 5),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							} else {
								var _v13 = _v1.a;
								var $temp$msg = A2($author$project$Main$UpdateHealth, model.j, 5),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							}
						default:
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				case 1:
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								c: A2(
									$elm$core$Dict$map,
									F2(
										function (id, player) {
											return $author$project$Main$createPlayer(
												_Utils_Tuple2(id, player.bA));
										}),
									model.c)
							}),
						$elm$core$Platform$Cmd$none);
				case 9:
					var id = msg.a;
					var healthDiff = msg.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								c: A3(
									$elm$core$Dict$update,
									id,
									$elm$core$Maybe$map(
										function (pInfo) {
											return _Utils_update(
												pInfo,
												{
													P: A2($author$project$Log$update, healthDiff, pInfo.P)
												});
										}),
									model.c)
							}),
						$author$project$Main$scrollLog('log'));
				case 10:
					var id = msg.a;
					var poisonDiff = msg.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								c: A3(
									$elm$core$Dict$update,
									id,
									$elm$core$Maybe$map(
										function (pInfo) {
											return _Utils_update(
												pInfo,
												{
													U: A2($author$project$Log$update, poisonDiff, pInfo.U)
												});
										}),
									model.c)
							}),
						$author$project$Main$scrollLog('log'));
				case 7:
					var name = msg.a;
					return _Utils_Tuple2(
						A2($author$project$Main$addPlayer, name, model),
						A2(
							$elm$core$Task$attempt,
							$elm$core$Basics$always($author$project$Main$Noop),
							$elm$browser$Browser$Dom$focus(
								'player-' + $elm$core$String$fromInt(model.G))));
				case 8:
					var id = msg.a;
					return _Utils_Tuple2(
						(_Utils_cmp(
							id,
							$elm$core$List$length(model.o)) < 0) ? _Utils_update(
							model,
							{j: id}) : model,
						$elm$core$Platform$Cmd$none);
				case 11:
					var mode = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{C: mode}),
						$elm$core$Platform$Cmd$none);
				case 3:
					var id = msg.a;
					var name = msg.b;
					var updatedPlayerInfo = A3(
						$elm$core$Dict$update,
						id,
						$elm$core$Maybe$map(
							function (p) {
								return _Utils_update(
									p,
									{bA: name});
							}),
						model.c);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{c: updatedPlayerInfo}),
						$elm$core$Platform$Cmd$none);
				case 4:
					var id = msg.a;
					var name = msg.b;
					var updatedPlayerInfo = A3(
						$elm$core$Dict$update,
						id,
						$elm$core$Maybe$map(
							function (p) {
								return _Utils_update(
									p,
									{az: name});
							}),
						model.c);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{c: updatedPlayerInfo}),
						$elm$core$Platform$Cmd$none);
				case 5:
					var selectedPlayer = msg.a;
					var selectedCommander = msg.b;
					var updatedPlayerInfo = A3(
						$elm$core$Dict$update,
						selectedPlayer,
						$elm$core$Maybe$map(
							function (p) {
								return _Utils_update(
									p,
									{al: selectedCommander});
							}),
						model.c);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{c: updatedPlayerInfo}),
						$elm$core$Platform$Cmd$none);
				default:
					var selectedPlayer = msg.a;
					var selectedCommander = msg.b;
					var diff = msg.c;
					var updateCommanderLogs = function (logs) {
						return A3(
							$elm$core$Dict$update,
							selectedCommander,
							$elm$core$Maybe$map(
								function (log) {
									return A2($author$project$Log$update, diff, log);
								}),
							logs);
					};
					var updatedPlayerInfo = A3(
						$elm$core$Dict$update,
						selectedPlayer,
						$elm$core$Maybe$map(
							function (p) {
								return _Utils_update(
									p,
									{
										D: updateCommanderLogs(p.D)
									});
							}),
						model.c);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{c: updatedPlayerInfo}),
						$elm$core$Platform$Cmd$none);
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Element$rgb = F3(
	function (r, g, b) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, r, g, b, 1);
	});
var $author$project$Main$accentCommander = {
	as: A3($mdgriffith$elm_ui$Element$rgb, 0.2, 0.5, 0.9)
};
var $mdgriffith$elm_ui$Element$rgb255 = F3(
	function (red, green, blue) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, 1);
	});
var $author$project$Main$accentCustom = {
	as: A3($mdgriffith$elm_ui$Element$rgb255, 201, 196, 191)
};
var $author$project$Main$accentHealth = {
	as: A3($mdgriffith$elm_ui$Element$rgb, 0.9, 0.3, 0.3)
};
var $author$project$Main$accentMana = {
	as: A3($mdgriffith$elm_ui$Element$rgb255, 174, 107, 77)
};
var $author$project$Main$accentPoison = {
	as: A3($mdgriffith$elm_ui$Element$rgb, 0.2, 0.8, 0.2)
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? $mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : $mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var $mdgriffith$elm_ui$Internal$Flag$borderColor = $mdgriffith$elm_ui$Internal$Flag$flag(28);
var $elm$core$Basics$round = _Basics_round;
var $mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return $elm$core$String$fromInt(
		$elm$core$Basics$round(x * 255));
};
var $mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return $mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var $mdgriffith$elm_ui$Element$Border$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'border-color',
			clr));
};
var $mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$AsColumn = 1;
var $mdgriffith$elm_ui$Internal$Model$asColumn = 1;
var $mdgriffith$elm_ui$Internal$Style$classes = {b7: 'a', aO: 'atv', b9: 'ab', ca: 'cx', cb: 'cy', cc: 'acb', cd: 'accx', ce: 'accy', cf: 'acr', ba: 'al', bb: 'ar', cg: 'at', aP: 'ah', aQ: 'av', ci: 's', cm: 'bh', cn: 'b', cr: 'w7', cs: 'bd', ct: 'bdt', at: 'bn', cu: 'bs', ax: 'cpe', cx: 'cp', cy: 'cpx', cz: 'cpy', A: 'c', aA: 'ctr', aB: 'cb', aC: 'ccx', B: 'ccy', aj: 'cl', aD: 'cr', cC: 'ct', cD: 'cptr', cE: 'ctxt', cK: 'fcs', bm: 'focus-within', cM: 'fs', cN: 'g', aU: 'hbh', aV: 'hc', bp: 'he', aW: 'hf', bq: 'hfp', cQ: 'hv', cS: 'ic', cU: 'fr', cW: 'iml', cX: 'imlf', cY: 'imlp', cZ: 'implw', c_: 'it', c$: 'i', bx: 'lnk', aa: 'nb', bD: 'notxt', c3: 'ol', c5: 'or', S: 'oq', c9: 'oh', bI: 'pg', bJ: 'p', da: 'ppe', dc: 'ui', r: 'r', df: 'sb', dg: 'sbx', dh: 'sby', di: 'sbt', dk: 'e', dm: 'cap', dn: 'sev', dt: 'sk', b_: 't', dw: 'tc', dx: 'w8', dy: 'w2', dz: 'w9', dA: 'tj', aI: 'tja', dB: 'tl', dC: 'w3', dD: 'w5', dE: 'w4', dF: 'tr', dG: 'w6', dH: 'w1', dI: 'tun', b0: 'ts', V: 'clr', dL: 'u', a6: 'wc', b4: 'we', a7: 'wf', b5: 'wfp', a8: 'wrp'};
var $mdgriffith$elm_ui$Internal$Model$Generic = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$div = $mdgriffith$elm_ui$Internal$Model$Generic;
var $mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$columnClass = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.A);
var $mdgriffith$elm_ui$Internal$Model$gridClass = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cN);
var $mdgriffith$elm_ui$Internal$Model$pageClass = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bI);
var $mdgriffith$elm_ui$Internal$Model$paragraphClass = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bJ);
var $mdgriffith$elm_ui$Internal$Model$rowClass = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.r);
var $mdgriffith$elm_ui$Internal$Model$singleClass = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dk);
var $mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Model$rowClass;
		case 1:
			return $mdgriffith$elm_ui$Internal$Model$columnClass;
		case 2:
			return $mdgriffith$elm_ui$Internal$Model$singleClass;
		case 3:
			return $mdgriffith$elm_ui$Internal$Model$gridClass;
		case 4:
			return $mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return $mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							$elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$AsEl = 2;
var $mdgriffith$elm_ui$Internal$Model$asEl = 2;
var $mdgriffith$elm_ui$Internal$Model$AsParagraph = 4;
var $mdgriffith$elm_ui$Internal$Model$asParagraph = 4;
var $mdgriffith$elm_ui$Internal$Flag$alignBottom = $mdgriffith$elm_ui$Internal$Flag$flag(41);
var $mdgriffith$elm_ui$Internal$Flag$alignRight = $mdgriffith$elm_ui$Internal$Flag$flag(40);
var $mdgriffith$elm_ui$Internal$Flag$centerX = $mdgriffith$elm_ui$Internal$Flag$flag(42);
var $mdgriffith$elm_ui$Internal$Flag$centerY = $mdgriffith$elm_ui$Internal$Flag$flag(43);
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 0:
			var px = x.a;
			return $elm$core$String$fromInt(px) + 'px';
		case 1:
			return 'auto';
		case 2:
			var i = x.a;
			return $elm$core$String$fromInt(i) + 'fr';
		case 3:
			var min = x.a;
			var len = x.b;
			return 'min' + ($elm$core$String$fromInt(min) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + ($elm$core$String$fromInt(max) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'mv-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			return $elm$core$Maybe$Just(
				'tfrm-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 13:
			var name = style.a;
			return name;
		case 12:
			var name = style.a;
			var o = style.b;
			return name;
		case 0:
			var _class = style.a;
			return _class;
		case 1:
			var name = style.a;
			return name;
		case 2:
			var i = style.a;
			return 'font-size-' + $elm$core$String$fromInt(i);
		case 3:
			var _class = style.a;
			return _class;
		case 4:
			var _class = style.a;
			return _class;
		case 5:
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 7:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 6:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 8:
			var template = style.a;
			return 'grid-rows-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.dd)) + ('-cols-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.u)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.$7.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.$7.b)))))));
		case 9:
			var pos = style.a;
			return 'gp grid-pos-' + ($elm$core$String$fromInt(pos.r) + ('-' + ($elm$core$String$fromInt(pos.cA) + ('-' + ($elm$core$String$fromInt(pos.dO) + ('-' + $elm$core$String$fromInt(pos.Z)))))));
		case 11:
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector) {
					case 0:
						return 'fs';
					case 1:
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (sty) {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_v1 === '') {
							return '';
						} else {
							var styleName = _v1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				$mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = $mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2($elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2($elm$core$Set$insert, styleName, cache),
			A2($elm$core$List$cons, style, existing));
	});
var $mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $mdgriffith$elm_ui$Internal$Model$formatColor = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return 'rgba(' + ($elm$core$String$fromInt(
		$elm$core$Basics$round(red * 255)) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(green * 255))) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(blue * 255))) + (',' + ($elm$core$String$fromFloat(alpha) + ')')))));
};
var $mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.bu ? $elm$core$Maybe$Just('inset') : $elm$core$Maybe$Nothing,
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.c2.a) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.c2.b) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.cp) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.dl) + 'px'),
					$elm$core$Maybe$Just(
					$mdgriffith$elm_ui$Internal$Model$formatColor(shadow.cB))
				])));
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bm) + ':focus-within',
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.aS),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.ck),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										cp: shadow.cp,
										cB: shadow.cB,
										bu: false,
										c2: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.c2)),
										dl: shadow.dl
									}));
						},
						focus.dj),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					]))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci) + (':focus .focusable, ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci) + '.focusable:focus')),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.aS),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.ck),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										cp: shadow.cp,
										cB: shadow.cB,
										bu: false,
										c2: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.c2)),
										dl: shadow.dl
									}));
						},
						focus.dj),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					])))
		]);
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Left = 3;
var $mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Right = 2;
var $mdgriffith$elm_ui$Internal$Style$Self = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Content = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Bottom = 1;
var $mdgriffith$elm_ui$Internal$Style$CenterX = 4;
var $mdgriffith$elm_ui$Internal$Style$CenterY = 5;
var $mdgriffith$elm_ui$Internal$Style$Top = 0;
var $mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cC);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aB);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aD);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aj);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aC);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.B);
	}
};
var $mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cg);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b9);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bb);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ba);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ca);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cb);
	}
};
var $mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _v0 = values(alignment);
		var content = _v0.a;
		var indiv = _v0.b;
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$contentName(alignment),
				content),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						indiv)
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aU),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cm),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.di),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b_),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a7),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aV),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a7),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b5),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a6),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		$mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment) {
				case 0:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 1:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 2:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 3:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 4:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var $mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						values(alignment))
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$Above = 0;
var $mdgriffith$elm_ui$Internal$Style$Behind = 5;
var $mdgriffith$elm_ui$Internal$Style$Below = 1;
var $mdgriffith$elm_ui$Internal$Style$OnLeft = 3;
var $mdgriffith$elm_ui$Internal$Style$OnRight = 2;
var $mdgriffith$elm_ui$Internal$Style$Within = 4;
var $mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = 0;
	var _v0 = function () {
		switch (loc) {
			case 0:
				return 0;
			case 1:
				return 0;
			case 2:
				return 0;
			case 3:
				return 0;
			case 4:
				return 0;
			default:
				return 0;
		}
	}();
	return _List_fromArray(
		[0, 1, 2, 3, 4, 5]);
}();
var $mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
			_Utils_ap(
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dk),
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cS))),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci) + ':focus',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dc),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cU),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aa),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aa),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dk),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				$mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2($elm$core$List$map, fn, $mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc) {
							case 0:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b7),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a7),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 1:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cn),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 2:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c5),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 3:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c3),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 4:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cU),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cm),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a8),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bD),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cD),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.da),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ax),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.V),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.S),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cQ, $mdgriffith$elm_ui$Internal$Style$classes.V)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cQ, $mdgriffith$elm_ui$Internal$Style$classes.S)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cK, $mdgriffith$elm_ui$Internal$Style$classes.V)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cK, $mdgriffith$elm_ui$Internal$Style$classes.S)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.aO, $mdgriffith$elm_ui$Internal$Style$classes.V)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.aO, $mdgriffith$elm_ui$Internal$Style$classes.S)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b0),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							$elm$core$String$join,
							', ',
							A2(
								$elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.df),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dg),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.r),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dh),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.A),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dk),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cx),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cy),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a6),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.at),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cs),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ct),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cu),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b_),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c_),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dk),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.r),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b4),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bx),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bq),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a7),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aA),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cf,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cd,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ca),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cd,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ca),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cd,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cb),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.cd + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.cf + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.cd)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dn),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.A),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bp),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.A),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a7),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b5),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a6),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cc,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ce,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cb),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ce,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cb),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ce,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cb),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.ce + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.cc + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.ce)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aA),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dn),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cN),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 1:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 2:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 3:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 4:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bI),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci + ':first-child'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.ci + ($mdgriffith$elm_ui$Internal$Style$selfName(3) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.ci))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.ci + ($mdgriffith$elm_ui$Internal$Style$selfName(2) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.ci))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cW),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background-color', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cZ),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dk),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'transparent')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bJ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aU),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cm),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b_),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dk),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cU),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cm),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b7),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cn),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c5),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c3),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b_),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dk),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Internal$Style$Child,
										$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b_),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
											]))
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.r),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.A),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cN),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dH),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dy),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dC),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dE),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dD),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dG),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cr),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dx),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c$),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dt),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dL),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dL),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dt)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dI),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dA),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dw),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dF),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dB),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var $mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var $mdgriffith$elm_ui$Internal$Style$commonValues = $elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			$elm$core$List$map,
			function (x) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + $elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							$elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 6)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 8, 32)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var $mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + ($mdgriffith$elm_ui$Internal$Style$classes.ci + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + ($mdgriffith$elm_ui$Internal$Style$classes.ci + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var $mdgriffith$elm_ui$Internal$Style$inputTextReset = '\ninput[type="search"],\ninput[type="search"]::-webkit-search-decoration,\ninput[type="search"]::-webkit-search-cancel-button,\ninput[type="search"]::-webkit-search-results-button,\ninput[type="search"]::-webkit-search-results-decoration {\n  -webkit-appearance:none;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$sliderReset = '\ninput[type=range] {\n  -webkit-appearance: none; \n  background: transparent;\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$thumbReset = '\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var $mdgriffith$elm_ui$Internal$Style$trackReset = '\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.r) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci) + (' { flex-basis: auto !important; } ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.r) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ci) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aA) + (' { flex-basis: auto !important; }}' + ($mdgriffith$elm_ui$Internal$Style$inputTextReset + ($mdgriffith$elm_ui$Internal$Style$sliderReset + ($mdgriffith$elm_ui$Internal$Style$trackReset + ($mdgriffith$elm_ui$Internal$Style$thumbReset + $mdgriffith$elm_ui$Internal$Style$explainer)))))))))))))));
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $mdgriffith$elm_ui$Internal$Style$Intermediate = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return {ay: closing, i: _List_Nil, I: _List_Nil, x: selector};
	});
var $mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_v0, rulesToRender) {
		var parent = _v0;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 0:
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								I: A2(
									$elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.I)
							});
					case 2:
						var _v2 = rule.a;
						var prop = _v2.a;
						var value = _v2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									{ay: '\n}', i: _List_Nil, I: props, x: '@supports (' + (prop + (':' + (value + (') {' + parent.x))))},
									rendered.i)
							});
					case 4:
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.x + (' + ' + selector), ''),
										adjRules),
									rendered.i)
							});
					case 1:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.x + (' > ' + child), ''),
										childRules),
									rendered.i)
							});
					case 3:
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											$mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.x, descriptor),
											''),
										descriptorRules),
									rendered.i)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.x, ''),
										batched),
									rendered.i)
							});
				}
			});
		return A3($elm$core$List$foldr, generateIntermediates, parent, rulesToRender);
	});
var $mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				function (_v3) {
					var x = _v3.a;
					var y = _v3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _v2 = rule.I;
		if (!_v2.b) {
			return '';
		} else {
			return rule.x + ('{' + (renderValues(rule.I) + (rule.ay + '}')));
		}
	};
	var renderIntermediate = function (_v0) {
		var rule = _v0;
		return _Utils_ap(
			renderClass(rule),
			$elm$core$String$concat(
				A2($elm$core$List$map, renderIntermediate, rule.i)));
	};
	return $elm$core$String$concat(
		A2(
			$elm$core$List$map,
			renderIntermediate,
			A3(
				$elm$core$List$foldr,
				F2(
					function (_v1, existing) {
						var name = _v1.a;
						var styleRules = _v1.b;
						return A2(
							$elm$core$List$cons,
							A2(
								$mdgriffith$elm_ui$Internal$Style$renderRules,
								A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var $mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	$mdgriffith$elm_ui$Internal$Style$overrides,
	$mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap($mdgriffith$elm_ui$Internal$Style$baseSheet, $mdgriffith$elm_ui$Internal$Style$commonValues)));
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $mdgriffith$elm_ui$Internal$Model$staticRoot = function (opts) {
	var _v0 = opts.c1;
	switch (_v0) {
		case 0:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'div',
				_List_Nil,
				_List_fromArray(
					[
						A3(
						$elm$virtual_dom$VirtualDom$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								$elm$virtual_dom$VirtualDom$text($mdgriffith$elm_ui$Internal$Style$rules)
							]))
					]));
		case 1:
			return $elm$virtual_dom$VirtualDom$text('');
		default:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'elm-ui-static-rules',
				_List_fromArray(
					[
						A2(
						$elm$virtual_dom$VirtualDom$property,
						'rules',
						$elm$json$Json$Encode$string($mdgriffith$elm_ui$Internal$Style$rules))
					]),
				_List_Nil);
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 0:
			return 'serif';
		case 1:
			return 'sans-serif';
		case 2:
			return 'monospace';
		case 3:
			var name = font.a;
			return '\"' + (name + '\"');
		case 4:
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.bA;
			return '\"' + (name + '\"');
	}
};
var $mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return name === 'smcp';
		case 1:
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var $mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.b1);
	} else {
		return false;
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _v0, existing) {
		var key = _v0.a;
		var val = _v0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var $mdgriffith$elm_ui$Internal$Model$renderStyle = F4(
	function (options, maybePseudo, selector, props) {
		if (maybePseudo.$ === 1) {
			return _List_fromArray(
				[
					selector + ('{' + (A3(
					$elm$core$List$foldl,
					$mdgriffith$elm_ui$Internal$Model$renderProps(false),
					'',
					props) + '\n}'))
				]);
		} else {
			var pseudo = maybePseudo.a;
			switch (pseudo) {
				case 1:
					var _v2 = options.cQ;
					switch (_v2) {
						case 0:
							return _List_Nil;
						case 2:
							return _List_fromArray(
								[
									selector + ('-hv {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(true),
									'',
									props) + '\n}'))
								]);
						default:
							return _List_fromArray(
								[
									selector + ('-hv:hover {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(false),
									'',
									props) + '\n}'))
								]);
					}
				case 0:
					var renderedProps = A3(
						$elm$core$List$foldl,
						$mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props);
					return _List_fromArray(
						[selector + ('-fs:focus {' + (renderedProps + '\n}')), '.' + ($mdgriffith$elm_ui$Internal$Style$classes.ci + (':focus ~ ' + (selector + ('-fs:not(.focus)  {' + (renderedProps + '\n}'))))), '.' + ($mdgriffith$elm_ui$Internal$Style$classes.ci + (':focus ' + (selector + ('-fs  {' + (renderedProps + '\n}'))))), selector + ('-fs:focus-within {' + (renderedProps + '\n}')), '.focusable-parent:focus ~ ' + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + (selector + ('-fs {' + (renderedProps + '\n}'))))))]);
				default:
					return _List_fromArray(
						[
							selector + ('-act:active {' + (A3(
							$elm$core$List$foldl,
							$mdgriffith$elm_ui$Internal$Model$renderProps(false),
							'',
							props) + '\n}'))
						]);
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return '\"' + (name + '\"');
		case 1:
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + $elm$core$String$fromInt(index)));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$renderVariant, font.b1)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'translate3d(' + ($elm$core$String$fromFloat(x) + ('px, ' + ($elm$core$String$fromFloat(y) + ('px, ' + ($elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + ($elm$core$String$fromFloat(tx) + ('px, ' + ($elm$core$String$fromFloat(ty) + ('px, ' + ($elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + ($elm$core$String$fromFloat(sx) + (', ' + ($elm$core$String$fromFloat(sy) + (', ' + ($elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + ($elm$core$String$fromFloat(ox) + (', ' + ($elm$core$String$fromFloat(oy) + (', ' + ($elm$core$String$fromFloat(oz) + (', ' + ($elm$core$String$fromFloat(angle) + 'rad)')))))));
			return $elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderStyleRule = F3(
	function (options, rule, maybePseudo) {
		switch (rule.$) {
			case 0:
				var selector = rule.a;
				var props = rule.b;
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, selector, props);
			case 13:
				var name = rule.a;
				var prop = rule.b;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
						]));
			case 12:
				var name = rule.a;
				var transparency = rule.b;
				var opacity = A2(
					$elm$core$Basics$max,
					0,
					A2($elm$core$Basics$min, 1, 1 - transparency));
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'opacity',
							$elm$core$String$fromFloat(opacity))
						]));
			case 2:
				var i = rule.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			case 1:
				var name = rule.a;
				var typefaces = rule.b;
				var features = A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
				var families = _List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-family',
						A2(
							$elm$core$String$join,
							', ',
							A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-variant',
						A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
					]);
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, '.' + name, families);
			case 3:
				var _class = rule.a;
				var prop = rule.b;
				var val = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, prop, val)
						]));
			case 4:
				var _class = rule.a;
				var prop = rule.b;
				var color = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							prop,
							$mdgriffith$elm_ui$Internal$Model$formatColor(color))
						]));
			case 5:
				var cls = rule.a;
				var x = rule.b;
				var y = rule.c;
				var yPx = $elm$core$String$fromInt(y) + 'px';
				var xPx = $elm$core$String$fromInt(x) + 'px';
				var single = '.' + $mdgriffith$elm_ui$Internal$Style$classes.dk;
				var row = '.' + $mdgriffith$elm_ui$Internal$Style$classes.r;
				var wrappedRow = '.' + ($mdgriffith$elm_ui$Internal$Style$classes.a8 + row);
				var right = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bb;
				var paragraph = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bJ;
				var page = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bI;
				var left = '.' + $mdgriffith$elm_ui$Internal$Style$classes.ba;
				var halfY = $elm$core$String$fromFloat(y / 2) + 'px';
				var halfX = $elm$core$String$fromFloat(x / 2) + 'px';
				var column = '.' + $mdgriffith$elm_ui$Internal$Style$classes.A;
				var _class = '.' + cls;
				var any = '.' + $mdgriffith$elm_ui$Internal$Style$classes.ci;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (row + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (wrappedRow + (' > ' + any)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (column + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_Utils_ap(_class, paragraph),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							'textarea' + (any + _class),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)')),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'height',
									'calc(100% + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::after'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-top',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::before'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-bottom',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								]))
						]));
			case 7:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'padding',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 6:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'border-width',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 8:
				var template = rule.a;
				var toGridLengthHelper = F3(
					function (minimum, maximum, x) {
						toGridLengthHelper:
						while (true) {
							switch (x.$) {
								case 0:
									var px = x.a;
									return $elm$core$String$fromInt(px) + 'px';
								case 1:
									var _v2 = _Utils_Tuple2(minimum, maximum);
									if (_v2.a.$ === 1) {
										if (_v2.b.$ === 1) {
											var _v3 = _v2.a;
											var _v4 = _v2.b;
											return 'max-content';
										} else {
											var _v6 = _v2.a;
											var maxSize = _v2.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v2.b.$ === 1) {
											var minSize = _v2.a.a;
											var _v5 = _v2.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
										} else {
											var minSize = _v2.a.a;
											var maxSize = _v2.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 2:
									var i = x.a;
									var _v7 = _Utils_Tuple2(minimum, maximum);
									if (_v7.a.$ === 1) {
										if (_v7.b.$ === 1) {
											var _v8 = _v7.a;
											var _v9 = _v7.b;
											return $elm$core$String$fromInt(i) + 'fr';
										} else {
											var _v11 = _v7.a;
											var maxSize = _v7.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v7.b.$ === 1) {
											var minSize = _v7.a.a;
											var _v10 = _v7.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
										} else {
											var minSize = _v7.a.a;
											var maxSize = _v7.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 3:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = $elm$core$Maybe$Just(m),
										$temp$maximum = maximum,
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
								default:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = minimum,
										$temp$maximum = $elm$core$Maybe$Just(m),
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
							}
						}
					});
				var toGridLength = function (x) {
					return A3(toGridLengthHelper, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, x);
				};
				var xSpacing = toGridLength(template.$7.a);
				var ySpacing = toGridLength(template.$7.b);
				var rows = function (x) {
					return 'grid-template-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.dd)));
				var msRows = function (x) {
					return '-ms-grid-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.u)));
				var msColumns = function (x) {
					return '-ms-grid-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.u)));
				var gapY = 'grid-row-gap:' + (toGridLength(template.$7.b) + ';');
				var gapX = 'grid-column-gap:' + (toGridLength(template.$7.a) + ';');
				var columns = function (x) {
					return 'grid-template-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.u)));
				var _class = '.grid-rows-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.dd)) + ('-cols-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.u)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.$7.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.$7.b)))))));
				var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msColumns + (msRows + '}')));
				return _List_fromArray(
					[base, supports]);
			case 9:
				var position = rule.a;
				var msPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'-ms-grid-row: ' + ($elm$core$String$fromInt(position.r) + ';'),
							'-ms-grid-row-span: ' + ($elm$core$String$fromInt(position.Z) + ';'),
							'-ms-grid-column: ' + ($elm$core$String$fromInt(position.cA) + ';'),
							'-ms-grid-column-span: ' + ($elm$core$String$fromInt(position.dO) + ';')
						]));
				var modernPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'grid-row: ' + ($elm$core$String$fromInt(position.r) + (' / ' + ($elm$core$String$fromInt(position.r + position.Z) + ';'))),
							'grid-column: ' + ($elm$core$String$fromInt(position.cA) + (' / ' + ($elm$core$String$fromInt(position.cA + position.dO) + ';')))
						]));
				var _class = '.grid-pos-' + ($elm$core$String$fromInt(position.r) + ('-' + ($elm$core$String$fromInt(position.cA) + ('-' + ($elm$core$String$fromInt(position.dO) + ('-' + $elm$core$String$fromInt(position.Z)))))));
				var modernGrid = _class + ('{' + (modernPosition + '}'));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msPosition + '}'));
				return _List_fromArray(
					[base, supports]);
			case 11:
				var _class = rule.a;
				var styles = rule.b;
				var renderPseudoRule = function (style) {
					return A3(
						$mdgriffith$elm_ui$Internal$Model$renderStyleRule,
						options,
						style,
						$elm$core$Maybe$Just(_class));
				};
				return A2($elm$core$List$concatMap, renderPseudoRule, styles);
			default:
				var transform = rule.a;
				var val = $mdgriffith$elm_ui$Internal$Model$transformValue(transform);
				var _class = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				var _v12 = _Utils_Tuple2(_class, val);
				if ((!_v12.a.$) && (!_v12.b.$)) {
					var cls = _v12.a.a;
					var v = _v12.b.a;
					return A4(
						$mdgriffith$elm_ui$Internal$Model$renderStyle,
						options,
						maybePseudo,
						'.' + cls,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
							]));
				} else {
					return _List_Nil;
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$encodeStyles = F2(
	function (options, stylesheet) {
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$map,
				function (style) {
					var styled = A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing);
					return _Utils_Tuple2(
						$mdgriffith$elm_ui$Internal$Model$getStyleName(style),
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styled));
				},
				stylesheet));
	});
var $mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_v0) {
			var name = _v0.a;
			var val = _v0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, renderPair, rules)) + '}'));
	});
var $mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _v0) {
		var parentAdj = _v0.a;
		var textAdjustment = _v0.b;
		return _List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.b_ + (', .' + (name + (' .' + (modifier + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.b_)))))))))), textAdjustment)
			]);
	});
var $mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _v0, otherFontName) {
		var full = _v0.a;
		var capital = _v0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_Utils_ap(
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.dm, capital),
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.cM, full)));
	});
var $mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.dm + (', ' + ('.' + (name + (' .' + $mdgriffith$elm_ui$Internal$Style$classes.dm))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.dm + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.b_ + (', .' + (name + (' .' + ($mdgriffith$elm_ui$Internal$Style$classes.dm + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.b_)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {Z: height / size, dl: size, b2: vertical};
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.cv, adjustment.cl, adjustment.cF, adjustment.c0]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.cF,
		$elm$core$List$minimum(lines));
	var newBaseline = A2(
		$elm$core$Maybe$withDefault,
		adjustment.cl,
		$elm$core$List$minimum(
			A2(
				$elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.cv,
		$elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		cv: A3($mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		bo: A3($mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var $mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				$elm$core$String$fromFloat(converted.Z)),
				_Utils_Tuple2(
				'vertical-align',
				$elm$core$String$fromFloat(converted.b2) + 'em'),
				_Utils_Tuple2(
				'font-size',
				$elm$core$String$fromFloat(converted.dl) + 'em')
			]));
};
var $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 1) {
					if (face.$ === 5) {
						var _with = face.a;
						var _v2 = _with.b8;
						if (_v2.$ === 1) {
							return found;
						} else {
							var adjustment = _v2.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.bo;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.cv;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		$elm$core$Maybe$Nothing,
		typefaces);
};
var $mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 4) {
			var url = font.b;
			return $elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_v2) {
		var name = _v2.a;
		var typefaces = _v2.b;
		var imports = A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2($elm$core$List$map, $elm$core$Tuple$first, rules);
	var fontAdjustments = function (_v1) {
		var name = _v1.a;
		var typefaces = _v1.b;
		var _v0 = $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_v0.$ === 1) {
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _v0.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					A2($mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontImports, rules)),
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontAdjustments, rules)));
};
var $mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 1) {
		var name = rule.a;
		var typefaces = rule.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var combine = F2(
			function (style, rendered) {
				return {
					aH: _Utils_ap(
						rendered.aH,
						A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing)),
					ao: function () {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_v1.$ === 1) {
							return rendered.ao;
						} else {
							var topLevel = _v1.a;
							return A2($elm$core$List$cons, topLevel, rendered.ao);
						}
					}()
				};
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			combine,
			{aH: _List_Nil, ao: _List_Nil},
			stylesheet);
		var topLevel = _v0.ao;
		var rules = _v0.aH;
		return _Utils_ap(
			$mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			$elm$core$String$concat(rules));
	});
var $mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		var _v0 = options.c1;
		switch (_v0) {
			case 0:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			case 1:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			default:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'elm-ui-rules',
					_List_fromArray(
						[
							A2(
							$elm$virtual_dom$VirtualDom$property,
							'rules',
							A2($mdgriffith$elm_ui$Internal$Model$encodeStyles, options, styleSheet))
						]),
					_List_Nil);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.cK)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'static-stylesheet',
				$mdgriffith$elm_ui$Internal$Model$staticRoot(opts)),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
				children)) : A2(
			$elm$core$List$cons,
			_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
			children);
	});
var $mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.cK)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			$mdgriffith$elm_ui$Internal$Model$staticRoot(opts),
			A2($elm$core$List$cons, dynamicStyleSheet, children)) : A2($elm$core$List$cons, dynamicStyleSheet, children);
	});
var $mdgriffith$elm_ui$Internal$Flag$heightBetween = $mdgriffith$elm_ui$Internal$Flag$flag(45);
var $mdgriffith$elm_ui$Internal$Flag$heightFill = $mdgriffith$elm_ui$Internal$Flag$flag(37);
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$core$Basics$not = _Basics_not;
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$core$Bitwise$and = _Bitwise_and;
var $mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _v0) {
		var fieldOne = _v0.a;
		var fieldTwo = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var $elm$html$Html$s = _VirtualDom_node('s');
var $elm$html$Html$u = _VirtualDom_node('u');
var $mdgriffith$elm_ui$Internal$Flag$widthBetween = $mdgriffith$elm_ui$Internal$Flag$flag(44);
var $mdgriffith$elm_ui$Internal$Flag$widthFill = $mdgriffith$elm_ui$Internal$Flag$flag(39);
var $mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 1) {
					var keyed = children.a;
					return A3(
						$elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return keyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return $elm$html$Html$div;
								case 'p':
									return $elm$html$Html$p;
								default:
									return $elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return unkeyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 0:
					return A2(createNode, 'div', attributes);
				case 1:
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						$elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dk))
									]))
							]));
			}
		}();
		switch (parentContext) {
			case 0:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ci, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.aA, $mdgriffith$elm_ui$Internal$Style$classes.B, $mdgriffith$elm_ui$Internal$Style$classes.cf])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ci, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.aA, $mdgriffith$elm_ui$Internal$Style$classes.B, $mdgriffith$elm_ui$Internal$Style$classes.cd])))
						]),
					_List_fromArray(
						[html])) : html));
			case 1:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ci, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.aA, $mdgriffith$elm_ui$Internal$Style$classes.ce])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.ci, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.aA, $mdgriffith$elm_ui$Internal$Style$classes.cc])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $mdgriffith$elm_ui$Internal$Model$textElementClasses = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.b_ + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.a6 + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aV)))));
var $mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$textElementFillClasses = $mdgriffith$elm_ui$Internal$Style$classes.ci + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.b_ + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.a7 + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aW)))));
var $mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementFillClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_v8, _v9) {
				var key = _v8.a;
				var child = _v8.b;
				var htmls = _v9.a;
				var existingStyles = _v9.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.cR, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.du : _Utils_ap(styled.du, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.cR, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.du : _Utils_ap(styled.du, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _v6) {
				var htmls = _v6.a;
				var existingStyles = _v6.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.cR, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.du : _Utils_ap(styled.du, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.cR, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.du : _Utils_ap(styled.du, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 1) {
			var keyedChildren = children.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _v1.a;
			var styles = _v1.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.du : _Utils_ap(rendered.du, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.O,
						rendered.R,
						rendered.K,
						$mdgriffith$elm_ui$Internal$Model$Keyed(
							A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.L)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						cR: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.O,
							rendered.R,
							rendered.K,
							$mdgriffith$elm_ui$Internal$Model$Keyed(
								A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.L))),
						du: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _v3 = A3(
				$elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _v3.a;
			var styles = _v3.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.du : _Utils_ap(rendered.du, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.O,
						rendered.R,
						rendered.K,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.L)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						cR: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.O,
							rendered.R,
							rendered.K,
							$mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.L))),
						du: allStyles
					});
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 10, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					function () {
						switch (location) {
							case 0:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aa, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.b7]));
							case 1:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aa, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.cn]));
							case 2:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aa, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.c5]));
							case 3:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aa, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.c3]));
							case 4:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aa, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.cU]));
							default:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aa, $mdgriffith$elm_ui$Internal$Style$classes.dk, $mdgriffith$elm_ui$Internal$Style$classes.cm]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 3:
							return $elm$virtual_dom$VirtualDom$text('');
						case 2:
							var str = elem.a;
							return $mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 0:
							var html = elem.a;
							return html($mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.cR, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, $mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2($mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 0:
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 1:
				var existingBehind = existing.a;
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2($elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 2:
				var existingInFront = existing.a;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2($elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2($elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2($elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 0:
				return $mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 1:
				var name = old.a;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.aP + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.ba);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.aP + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bb);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.aP + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.ca);
	}
};
var $mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.aQ + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cg);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.aQ + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.b9);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.aQ + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cb);
	}
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 0:
				switch (component.$) {
					case 0:
						var x = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 1:
						var y = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 2:
						var z = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 1:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 1:
						var newY = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 2:
						var newZ = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 1:
						var newY = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 2:
						var newZ = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 3:
						var newMove = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 4:
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$height = $mdgriffith$elm_ui$Internal$Flag$flag(7);
var $mdgriffith$elm_ui$Internal$Flag$heightContent = $mdgriffith$elm_ui$Internal$Flag$flag(36);
var $mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_v0, _v1) {
		var one = _v0.a;
		var two = _v0.b;
		var three = _v1.a;
		var four = _v1.b;
		return A2($mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var $mdgriffith$elm_ui$Internal$Flag$none = A2($mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var $mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 0:
			var px = h.a;
			var val = $elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.bp + (' ' + name),
				_List_fromArray(
					[
						A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.aV,
				_List_Nil);
		case 2:
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.aW,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bq + (' height-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.ci + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.A + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$widthContent = $mdgriffith$elm_ui$Internal$Flag$flag(38);
var $mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 0:
			var px = w.a;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.b4 + (' width-px-' + $elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + $elm$core$String$fromInt(px),
						'width',
						$elm$core$String$fromInt(px) + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.a6,
				_List_Nil);
		case 2:
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.a7,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.b5 + (' width-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.ci + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.r + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$borderWidth = $mdgriffith$elm_ui$Internal$Flag$flag(27);
var $elm$core$Basics$ge = _Utils_ge;
var $mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, $mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 3) {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 2:
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 7:
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$width = $mdgriffith$elm_ui$Internal$Flag$flag(6);
var $mdgriffith$elm_ui$Internal$Flag$xAlign = $mdgriffith$elm_ui$Internal$Flag$flag(30);
var $mdgriffith$elm_ui$Internal$Flag$yAlign = $mdgriffith$elm_ui$Internal$Flag$flag(29);
var $mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _v1 = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_v1.$ === 1) {
					return {
						K: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes),
							attrs),
						L: children,
						O: has,
						R: node,
						du: styles
					};
				} else {
					var _class = _v1.a;
					return {
						K: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						L: children,
						O: has,
						R: node,
						du: A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 0:
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 3:
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 1:
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2($elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 4:
						var flag = attribute.a;
						var style = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2($mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2($elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 10:
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2($mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 7:
						var width = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 0:
									var px = width.a;
									var $temp$classes = ($mdgriffith$elm_ui$Internal$Style$classes.b4 + (' width-px-' + $elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3(
											$mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + $elm$core$String$fromInt(px),
											'width',
											$elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.a6),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.a7),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.b5 + (' width-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.ci + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.r + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v4 = $mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _v4.a;
									var newClass = _v4.b;
									var newStyles = _v4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 8:
						var height = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 0:
									var px = height.a;
									var val = $elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.bp + (' ' + (name + (' ' + classes))),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.aV + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.aW + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.bq + (' height-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.ci + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.A + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v6 = $mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _v6.a;
									var newClass = _v6.b;
									var newStyles = _v6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 2:
						var description = attribute.a;
						switch (description.$) {
							case 0:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 1:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 2:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 3:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 4:
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											$mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + $elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 9:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 8:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 5:
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 6:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 9:
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 3:
									return styles;
								case 2:
									var str = elem.a;
									return styles;
								case 0:
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.du);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3($mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 6:
						var x = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$untransformed = $mdgriffith$elm_ui$Internal$Model$Untransformed;
var $mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			$mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				$mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				$mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				$mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				$elm$core$List$reverse(attributes)));
	});
var $mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 8, a: a};
};
var $mdgriffith$elm_ui$Element$height = $mdgriffith$elm_ui$Internal$Model$Height;
var $mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		$elm$html$Html$Attributes$class(cls));
};
var $mdgriffith$elm_ui$Internal$Model$Content = {$: 1};
var $mdgriffith$elm_ui$Element$shrink = $mdgriffith$elm_ui$Internal$Model$Content;
var $mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 7, a: a};
};
var $mdgriffith$elm_ui$Element$width = $mdgriffith$elm_ui$Internal$Model$Width;
var $mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cC + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aj)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					attrs)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var $mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$fill = $mdgriffith$elm_ui$Internal$Model$Fill(1);
var $mdgriffith$elm_ui$Element$fillPortion = $mdgriffith$elm_ui$Internal$Model$Fill;
var $mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$AllowHover = 1;
var $mdgriffith$elm_ui$Internal$Model$Layout = 0;
var $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	ck: $elm$core$Maybe$Nothing,
	aS: $elm$core$Maybe$Nothing,
	dj: $elm$core$Maybe$Just(
		{
			cp: 0,
			cB: A4($mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			c2: _Utils_Tuple2(0, 0),
			dl: 3
		})
};
var $mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 0:
					var hoverable = opt.a;
					var _v4 = record.cQ;
					if (_v4.$ === 1) {
						return _Utils_update(
							record,
							{
								cQ: $elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 1:
					var focusStyle = opt.a;
					var _v5 = record.cK;
					if (_v5.$ === 1) {
						return _Utils_update(
							record,
							{
								cK: $elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _v6 = record.c1;
					if (_v6.$ === 1) {
						return _Utils_update(
							record,
							{
								c1: $elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			cK: function () {
				var _v0 = record.cK;
				if (_v0.$ === 1) {
					return $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _v0.a;
					return focusable;
				}
			}(),
			cQ: function () {
				var _v1 = record.cQ;
				if (_v1.$ === 1) {
					return 1;
				} else {
					var hoverable = _v1.a;
					return hoverable;
				}
			}(),
			c1: function () {
				var _v2 = record.c1;
				if (_v2.$ === 1) {
					return 0;
				} else {
					var actualMode = _v2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			$elm$core$List$foldr,
			combine,
			{cK: $elm$core$Maybe$Nothing, cQ: $elm$core$Maybe$Nothing, c1: $elm$core$Maybe$Nothing},
			options));
};
var $mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 0:
				var html = el.a;
				return html($mdgriffith$elm_ui$Internal$Model$asEl);
			case 1:
				var styles = el.a.du;
				var html = el.a.cR;
				return A2(
					html,
					mode(styles),
					$mdgriffith$elm_ui$Internal$Model$asEl);
			case 2:
				var text = el.a;
				return $mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return $mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = $mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _v0 = options.c1;
			if (_v0 === 1) {
				return $mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var $mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 3, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$bgColor = $mdgriffith$elm_ui$Internal$Flag$flag(8);
var $mdgriffith$elm_ui$Internal$Flag$fontColor = $mdgriffith$elm_ui$Internal$Flag$flag(14);
var $mdgriffith$elm_ui$Internal$Flag$fontFamily = $mdgriffith$elm_ui$Internal$Flag$flag(5);
var $mdgriffith$elm_ui$Internal$Flag$fontSize = $mdgriffith$elm_ui$Internal$Flag$flag(4);
var $elm$core$String$toLower = _String_toLower;
var $elm$core$String$words = _String_words;
var $mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 0:
						return 'serif';
					case 1:
						return 'sans-serif';
					case 2:
						return 'monospace';
					case 3:
						var name = font.a;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					case 4:
						var name = font.a;
						var url = font.b;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					default:
						var name = font.a.bA;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
				}
			}());
	});
var $mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			$mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			$mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontSize,
			$mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				$mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var $mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_v0, attrs, child) {
		var options = _v0.bG;
		return A3(
			$mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[$mdgriffith$elm_ui$Internal$Style$classes.dc, $mdgriffith$elm_ui$Internal$Style$classes.ci, $mdgriffith$elm_ui$Internal$Style$classes.dk]))),
				_Utils_ap($mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var $mdgriffith$elm_ui$Element$layout = $mdgriffith$elm_ui$Element$layoutWith(
	{bG: _List_Nil});
var $mdgriffith$elm_ui$Internal$Flag$borderRound = $mdgriffith$elm_ui$Internal$Flag$flag(17);
var $mdgriffith$elm_ui$Element$Border$rounded = function (radius) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + $elm$core$String$fromInt(radius),
			'border-radius',
			$elm$core$String$fromInt(radius) + 'px'));
};
var $mdgriffith$elm_ui$Internal$Model$AsRow = 0;
var $mdgriffith$elm_ui$Internal$Model$asRow = 0;
var $mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asRow,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aj + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.B)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$borderStyle = $mdgriffith$elm_ui$Internal$Flag$flag(11);
var $mdgriffith$elm_ui$Element$Border$solid = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$borderStyle, $mdgriffith$elm_ui$Internal$Style$classes.cu);
var $mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$text = function (content) {
	return $mdgriffith$elm_ui$Internal$Model$Text(content);
};
var $author$project$Main$logTheme = function (accent) {
	return {as: accent.as};
};
var $author$project$Log$history = function (_v0) {
	var initial = _v0.aX;
	var updates = _v0.ar;
	return _Utils_ap(
		updates,
		_List_fromArray(
			[
				_Utils_Tuple2(0, initial)
			]));
};
var $mdgriffith$elm_ui$Element$htmlAttribute = $mdgriffith$elm_ui$Internal$Model$Attr;
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $mdgriffith$elm_ui$Internal$Flag$overflow = $mdgriffith$elm_ui$Internal$Flag$flag(20);
var $mdgriffith$elm_ui$Element$scrollbarX = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.dg);
var $mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 6, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Right = 2;
var $mdgriffith$elm_ui$Element$alignRight = $mdgriffith$elm_ui$Internal$Model$AlignX(2);
var $mdgriffith$elm_ui$Internal$Flag$fontAlignment = $mdgriffith$elm_ui$Internal$Flag$flag(12);
var $mdgriffith$elm_ui$Element$Font$alignRight = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.dF);
var $mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 7, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Flag$padding = $mdgriffith$elm_ui$Internal$Flag$flag(2);
var $mdgriffith$elm_ui$Element$paddingXY = F2(
	function (x, y) {
		return _Utils_eq(x, y) ? A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + $elm$core$String$fromInt(x),
				x,
				x,
				x,
				x)) : A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var $mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontSize,
		$mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var $mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 6, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Element$Border$width = function (v) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + $elm$core$String$fromInt(v),
			v,
			v,
			v,
			v));
};
var $author$project$Log$viewAccumItem = F2(
	function (theme, _int) {
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Border$width(4),
					$mdgriffith$elm_ui$Element$Border$solid,
					$mdgriffith$elm_ui$Element$Border$color(theme.as),
					$mdgriffith$elm_ui$Element$Font$size(48),
					$mdgriffith$elm_ui$Element$Font$alignRight,
					A2($mdgriffith$elm_ui$Element$paddingXY, 20, 20)
				]),
			$mdgriffith$elm_ui$Element$text(
				$elm$core$String$fromInt(_int)));
	});
var $mdgriffith$elm_ui$Element$Font$color = function (fontColor) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(fontColor),
			'color',
			fontColor));
};
var $author$project$Log$viewDiffItem = F2(
	function (theme, diff) {
		var positive = diff >= 0;
		var negative = diff < 0;
		var natural = diff > 0;
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Border$width(4),
					$mdgriffith$elm_ui$Element$Border$solid,
					$mdgriffith$elm_ui$Element$Border$color(theme.as),
					$mdgriffith$elm_ui$Element$Font$size(48),
					$mdgriffith$elm_ui$Element$Font$alignRight,
					A2($mdgriffith$elm_ui$Element$paddingXY, 20, 20),
					natural ? $mdgriffith$elm_ui$Element$Font$color(
					A3($mdgriffith$elm_ui$Element$rgb, 0.3, 0.9, 0.3)) : (negative ? $mdgriffith$elm_ui$Element$Font$color(
					A3($mdgriffith$elm_ui$Element$rgb, 0.9, 0.3, 0.3)) : $mdgriffith$elm_ui$Element$Font$color(
					A3($mdgriffith$elm_ui$Element$rgb, 0, 0, 0)))
				]),
			$mdgriffith$elm_ui$Element$text(
				_Utils_ap(
					positive ? '+' : '',
					$elm$core$String$fromInt(diff))));
	});
var $author$project$Log$viewItemColumn = F2(
	function (theme, _v0) {
		var diff = _v0.a;
		var accum = _v0.b;
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$alignRight
				]),
			_List_fromArray(
				[
					A2($author$project$Log$viewDiffItem, theme, diff),
					A2($author$project$Log$viewAccumItem, theme, accum)
				]));
	});
var $author$project$Log$viewLog = F3(
	function (theme, id, log) {
		var items = $elm$core$List$reverse(
			$author$project$Log$history(log));
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Border$color(theme.as),
					$mdgriffith$elm_ui$Element$Border$solid,
					$mdgriffith$elm_ui$Element$Border$width(4),
					$mdgriffith$elm_ui$Element$scrollbarX,
					$mdgriffith$elm_ui$Element$htmlAttribute(
					$elm$html$Html$Attributes$id(id))
				]),
			A2(
				$elm$core$List$map,
				$author$project$Log$viewItemColumn(theme),
				items));
	});
var $author$project$Main$viewActiveLog = F2(
	function (accent, model) {
		var selectedLog = function (player) {
			var _v0 = model.C;
			if (_v0 === 1) {
				return player.U;
			} else {
				return player.P;
			}
		};
		var maybeSelectedPlayer = A2(
			$elm$core$Maybe$andThen,
			function (id) {
				return A2($elm$core$Dict$get, id, model.c);
			},
			$elm$core$List$head(
				A2($elm$core$List$drop, model.j, model.o)));
		return A2(
			$elm$core$Maybe$withDefault,
			$mdgriffith$elm_ui$Element$text('Unable to render log'),
			A2(
				$elm$core$Maybe$map,
				A2(
					$author$project$Log$viewLog,
					$author$project$Main$logTheme(accent),
					'log'),
				A2($elm$core$Maybe$map, selectedLog, maybeSelectedPlayer)));
	});
var $author$project$Main$Commander = 2;
var $author$project$Main$Custom = 4;
var $author$project$Main$Mana = 3;
var $author$project$Main$Poison = 1;
var $author$project$Main$SetMode = function (a) {
	return {$: 11, a: a};
};
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$metadata = $elm$svg$Svg$trustedNode('metadata');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$rgb = F3(
	function (r, g, b) {
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, 1.0);
	});
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $elm$svg$Svg$Attributes$version = _VirtualDom_attribute('version');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$Icons$commander = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$id('svg8'),
			$elm$svg$Svg$Attributes$version('1.1'),
			$elm$svg$Svg$Attributes$viewBox('0 0 142.30072 153.80157'),
			$elm$svg$Svg$Attributes$height('64'),
			$elm$svg$Svg$Attributes$width('64')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$defs,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('defs2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$metadata,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('metadata5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$transform('translate(-41.080975,-82.206446)'),
					$elm$svg$Svg$Attributes$id('layer1'),
					$elm$svg$Svg$Attributes$fill(
					$avh4$elm_color$Color$toCssString(
						A3($avh4$elm_color$Color$rgb, 0.2, 0.5, 0.9)))
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$id('path865'),
							$elm$svg$Svg$Attributes$d('m 41.257855,235.88575 c -0.0973,-0.0973 -0.17688,-5.73839 -0.17688,-12.5358 0,-9.57207 0.0746,-12.41755 0.33073,-12.61894 0.1819,-0.14301 16.19388,-8.68244 35.58217,-18.97652 l 35.251445,-18.71649 35.26002,18.71842 c 19.39301,10.29513 35.41,18.83552 35.59331,18.97865 0.26137,0.20407 0.31847,2.95066 0.26459,12.72726 l -0.0687,12.46703 -70.92989,0.0666 c -39.011445,0.0366 -71.009495,-0.013 -71.106775,-0.11024 z m 3.79187,-72.68076 v -32.11083 l 15.37746,-7.37823 c 8.45761,-4.05803 15.48277,-7.41808 15.61148,-7.46678 0.13506,-0.0511 0.20559,13.53775 0.16681,32.13601 l -0.0672,32.22456 -15.34584,7.34866 c -8.44021,4.04176 -15.43513,7.35063 -15.54427,7.35305 -0.10914,0.002 -0.19844,-14.44548 -0.19844,-32.10644 z m 118.665635,24.74623 -15.34584,-7.35265 -0.0672,-32.19659 c -0.037,-17.70812 -0.0316,-32.19658 0.0118,-32.19658 0.0435,0 7.03886,3.33325 15.54532,7.40721 l 15.46629,7.40721 0.0672,32.148 c 0.037,17.6814 -0.0226,32.14531 -0.13229,32.14203 -0.10972,-0.003 -7.10512,-3.31467 -15.54532,-7.35863 z m -77.917055,-52.2328 0.003,-39.75364 12.98731,-6.879167 c 7.143025,-3.783542 13.201765,-6.879167 13.463855,-6.879167 0.2621,0 6.32132,3.095625 13.46492,6.879167 l 12.98838,6.879167 0.003,39.75364 c 0.002,21.86451 -0.0754,39.75365 -0.17172,39.75365 -0.0963,0 -5.96012,-3.06586 -13.03073,-6.81302 -7.07061,-3.74716 -13.03425,-6.81302 -13.25253,-6.81302 -0.21828,0 -6.18547,3.06586 -13.260415,6.81302 -7.07494,3.74716 -12.93877,6.81302 -13.03073,6.81302 -0.092,0 -0.16596,-17.88914 -0.16445,-39.75365 z')
						]),
					_List_Nil)
				]))
		]));
var $elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var $author$project$Icons$counters = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$id('svg8'),
			$elm$svg$Svg$Attributes$version('1.1'),
			$elm$svg$Svg$Attributes$viewBox('0 0 158.75 158.75'),
			$elm$svg$Svg$Attributes$height('64'),
			$elm$svg$Svg$Attributes$width('64')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$defs,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('defs2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$metadata,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('metadata5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$transform('translate(-77.44311,-19.442841)'),
					$elm$svg$Svg$Attributes$id('layer1')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$transform('translate(6.2060812,127.18329)'),
							$elm$svg$Svg$Attributes$id('g1089')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$g,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$transform('matrix(0.26458333,0,0,0.26458333,21.364114,-256.04852)'),
									$elm$svg$Svg$Attributes$id('g1187')
								]),
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$g,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$transform('translate(186.49056,598.74923)'),
											$elm$svg$Svg$Attributes$id('layer1-3')
										]),
									_List_fromArray(
										[
											A2(
											$elm$svg$Svg$g,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$transform('matrix(6,0,0,6,5042.0055,-38.21478)'),
													$elm$svg$Svg$Attributes$id('g159')
												]),
											_List_fromArray(
												[
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$d('M -740,49.998 C -740,77.613 -762.383,100 -790,100 -817.613,100 -840,77.613 -840,49.998 -840,22.385 -817.613,0 -790,0 c 27.617,0 50,22.385 50,49.998 z'),
															$elm$svg$Svg$Attributes$id('path161'),
															$elm$svg$Svg$Attributes$style('fill:#cac5c0')
														]),
													_List_Nil)
												])),
											A2(
											$elm$svg$Svg$g,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$transform('matrix(6,0,0,6,5042.0055,-38.21478)'),
													$elm$svg$Svg$Attributes$id('g163')
												]),
											_List_fromArray(
												[
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$d('m -783.359,80 v -3.297 c 4.703,-0.521 7.059,-1.887 7.059,-4.082 0,-1.582 -0.629,-3.164 -1.883,-4.746 l -13.008,-16.01 -10.273,12.111 c -2.57,2.973 -3.863,5.432 -3.863,7.354 0,2.976 2.354,4.771 7.07,5.387 V 80 h -25.051 v -3.287 c 3.172,-0.609 5.867,-1.752 8.102,-3.416 1.539,-1.223 3.637,-3.371 6.285,-6.438 l 15.449,-17.915 -17.867,-21.66 c -2.315,-2.797 -4.121,-4.633 -5.401,-5.51 -1.724,-1.225 -4.121,-2.012 -7.203,-2.361 V 16 h 29.539 v 3.285 c -4.453,0.877 -6.676,2.234 -6.676,4.074 0,1.23 0.594,2.586 1.793,4.072 l 11.816,14.322 9.508,-11.691 c 1.711,-2.104 2.57,-3.941 2.57,-5.52 0,-2.629 -2.105,-4.34 -6.301,-5.129 V 16 h 23.512 v 3.291 c -2.055,0.439 -3.473,0.834 -4.23,1.188 -2.57,1.229 -6.496,4.869 -11.797,10.92 -0.852,0.969 -4.453,5.393 -10.789,13.285 l 18.977,23.363 c 4.113,5.078 8.305,7.971 12.586,8.666 V 80 Z'),
															$elm$svg$Svg$Attributes$id('path165'),
															$elm$svg$Svg$Attributes$style('fill:#0d0f0f')
														]),
													_List_Nil)
												]))
										]))
								]))
						]))
				]))
		]));
var $author$project$Icons$health = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$id('svg8'),
			$elm$svg$Svg$Attributes$version('1.1'),
			$elm$svg$Svg$Attributes$viewBox('0 0 345.26889 345.2749'),
			$elm$svg$Svg$Attributes$height('64'),
			$elm$svg$Svg$Attributes$width('64')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$defs,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('defs2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$metadata,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('metadata5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$transform('translate(19.497741,94.860927)'),
					$elm$svg$Svg$Attributes$id('layer1')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$transform('translate(6.2060812,127.18329)'),
							$elm$svg$Svg$Attributes$id('g1089'),
							$elm$svg$Svg$Attributes$fill(
							$avh4$elm_color$Color$toCssString(
								A3($avh4$elm_color$Color$rgb, 0.9, 0.3, 0.3)))
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1105'),
									$elm$svg$Svg$Attributes$d('m 41.850241,123.1625 c 0.927488,-0.0729 2.356238,-0.072 3.175,0.002 0.818765,0.074 0.05991,0.13364 -1.686338,0.13256 -1.74625,-0.001 -2.416147,-0.0616 -1.488662,-0.13452 z m 202.141669,0 c 0.92749,-0.0729 2.35624,-0.072 3.175,0.002 0.81877,0.074 0.0599,0.13364 -1.68634,0.13256 -1.74625,-0.001 -2.41614,-0.0616 -1.48866,-0.13452 z M 15.028486,122.82628 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339548,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679098,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 259.291674,0 c 0.33955,-0.0887 0.89517,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16134 -0.61736,0.16134 -0.6791,0 -0.95691,-0.0726 -0.61736,-0.16134 z m 30.95625,-11.1789 c 0,-0.0733 0.27781,-0.35107 0.61736,-0.61736 0.55953,-0.43881 0.572,-0.42633 0.13319,0.1332 -0.46085,0.58763 -0.75055,0.77451 -0.75055,0.48416 z M -25.573452,80.383356 c 0,-0.48507 0.08007,-0.683507 0.177936,-0.440973 0.09787,0.242535 0.09787,0.63941 0,0.881945 -0.09786,0.242534 -0.177936,0.0441 -0.177936,-0.440972 z M 319.47648,69.623634 c 0.004,-0.776111 0.0762,-1.052072 0.16055,-0.613248 0.0844,0.438824 0.0812,1.073824 -0.007,1.411111 -0.0883,0.337288 -0.15734,-0.02177 -0.15342,-0.797863 z M 187.58059,53.043079 c 7.1e-4,-2.134305 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z M -25.866171,50.573635 c 0.0016,-1.358195 0.06533,-1.87059 0.141696,-1.138661 0.07637,0.731933 0.07509,1.843183 -0.0028,2.469444 -0.07793,0.626266 -0.140415,0.02741 -0.13885,-1.330783 z M 106.0779,21.998636 c 0.001,-1.552222 0.0633,-2.14334 0.13782,-1.313592 0.0745,0.829747 0.0735,2.099747 -0.002,2.822222 -0.0758,0.722474 -0.13677,0.04359 -0.13549,-1.50863 z M 69.369145,-8.7777236 c 1.122743,-0.07022 2.868993,-0.069568 3.880555,0.00141 1.011562,0.071018 0.09295,0.128471 -2.041352,0.1276738 -2.134306,-7.056e-4 -2.961947,-0.0589 -1.839203,-0.1291202 z m 151.694445,0 c 1.12275,-0.07022 2.869,-0.069568 3.88056,0.00141 1.01156,0.071018 0.093,0.128471 -2.04135,0.1276738 -2.13431,-7.056e-4 -2.96195,-0.0589 -1.83921,-0.1291202 z M 47.307653,-90.270765 c 1.600729,-0.06537 4.220104,-0.06537 5.820833,0 1.60073,0.06537 0.291042,0.11885 -2.910416,0.11885 -3.201459,0 -4.511146,-0.05348 -2.910417,-0.11885 z m 193.675007,0 c 1.60073,-0.06537 4.2201,-0.06537 5.82083,0 1.60073,0.06537 0.29104,0.11885 -2.91042,0.11885 -3.20145,0 -4.51114,-0.05348 -2.91041,-0.11885 z M 106.07557,-121.05275 c 0,-1.4552 0.0623,-2.05052 0.13838,-1.32291 0.0761,0.7276 0.0761,1.91823 0,2.64583 -0.0761,0.72761 -0.13838,0.13229 -0.13838,-1.32292 z m 213.4408,-19.22638 c 0.001,-1.94028 0.0602,-2.68902 0.13164,-1.66387 0.0715,1.02515 0.0707,2.61265 -0.002,3.52778 -0.0723,0.91512 -0.1308,0.0764 -0.12988,-1.86391 z m -345.388987,-4.23334 c 0.002,-1.16416 0.06784,-1.59788 0.146354,-0.96382 0.07851,0.63407 0.0769,1.58657 -0.0036,2.11667 -0.08049,0.5301 -0.144734,0.0113 -0.142754,-1.15285 z m 213.454657,-7.23194 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -213.142421,-24.51805 c 0.0072,-0.58209 0.08625,-0.77751 0.175699,-0.43426 0.08944,0.34324 0.08356,0.81949 -0.01307,1.05833 -0.09663,0.23884 -0.169816,-0.042 -0.162627,-0.62407 z m 344.650821,-0.17639 c 0,-0.48507 0.0801,-0.68351 0.17794,-0.44098 0.0979,0.24254 0.0979,0.63941 0,0.88195 -0.0979,0.24253 -0.17794,0.0441 -0.17794,-0.44097 z M 14.32293,-221.83759 c 0.339549,-0.0887 0.895174,-0.0887 1.234722,0 0.339549,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679097,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 266.54567,0.002 c 0.25466,-0.10191 0.55893,-0.0894 0.67615,0.0279 0.11723,0.11722 -0.0911,0.2006 -0.46302,0.18528 -0.41096,-0.0169 -0.49456,-0.10052 -0.21313,-0.21313 z m -237.429678,-0.37229 c 1.025152,-0.0715 2.612652,-0.0707 3.527778,0.002 0.915127,0.0724 0.07637,0.1309 -1.863908,0.12998 -1.940278,-9.2e-4 -2.689021,-0.0602 -1.66387,-0.13165 z m 212.369728,0.002 c 0.82975,-0.0745 2.09975,-0.0735 2.82222,0.002 0.72248,0.0758 0.0436,0.13678 -1.50863,0.1355 -1.55222,-0.001 -2.14334,-0.0633 -1.31359,-0.13783 z'),
									$elm$svg$Svg$Attributes$style('fill:#d0d0d0;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1103'),
									$elm$svg$Svg$Attributes$d('m 47.131264,123.16957 c 0.533576,-0.0808 1.406702,-0.0808 1.940278,0 0.533576,0.0808 0.09701,0.14696 -0.970139,0.14696 -1.067153,0 -1.503715,-0.0661 -0.970139,-0.14696 z m 192.974296,-3.6e-4 c 0.53625,-0.0811 1.33,-0.0789 1.76389,0.005 0.4339,0.0837 -0.005,0.15008 -0.97499,0.14743 -0.97014,-0.003 -1.32514,-0.0712 -0.7889,-0.15225 z M 16.968763,122.83583 c 0.242535,-0.0979 0.63941,-0.0979 0.881945,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.48507,0 -0.683507,-0.0801 -0.440973,-0.17793 z m 256.138717,-0.007 c 0.25466,-0.10191 0.55894,-0.0894 0.67616,0.0278 0.11723,0.11723 -0.0911,0.20061 -0.46302,0.18529 -0.41097,-0.0169 -0.49456,-0.10052 -0.21314,-0.21314 z m 33.22726,-12.23944 c 0,-0.0733 0.27781,-0.35107 0.61736,-0.61736 0.55953,-0.43881 0.57201,-0.42633 0.1332,0.13319 -0.46086,0.58764 -0.75056,0.77452 -0.75056,0.48417 z M -25.560381,78.795856 c 0.0072,-0.582084 0.08625,-0.777501 0.175699,-0.434259 0.08944,0.343242 0.08356,0.819492 -0.01307,1.058333 -0.09663,0.238841 -0.169816,-0.04199 -0.162627,-0.624074 z M 319.47648,67.15419 c 0.004,-0.776111 0.0762,-1.052072 0.16055,-0.613248 0.0844,0.438824 0.0812,1.073824 -0.007,1.411111 -0.0883,0.337287 -0.15734,-0.02177 -0.15342,-0.797863 z M 187.58204,60.627801 c 0,-2.231319 0.0573,-3.144132 0.12729,-2.028472 0.07,1.11566 0.07,2.941284 0,4.056944 -0.07,1.11566 -0.12729,0.202847 -0.12729,-2.028472 z M -25.866171,45.987524 c 0.0016,-1.358194 0.06533,-1.87059 0.141696,-1.138661 0.07637,0.731933 0.07509,1.843183 -0.0028,2.469445 -0.07793,0.626265 -0.140415,0.02741 -0.13885,-1.330784 z M 106.07557,27.113914 c 0,-1.455208 0.0623,-2.050521 0.13838,-1.322917 0.0761,0.727604 0.0761,1.918229 0,2.645833 -0.0761,0.727605 -0.13838,0.132292 -0.13838,-1.322916 z m 81.84677,-32.6319429 c 0.001,-1.5522222 0.0633,-2.1433401 0.13783,-1.3135927 0.0745,0.8297474 0.0735,2.0997473 -0.002,2.8222221 -0.0758,0.7224747 -0.13678,0.043593 -0.1355,-1.5086294 z M 63.880342,-8.7712502 c 0.626262,-0.077932 1.737512,-0.079213 2.469444,-0.00282 0.73193,0.076369 0.219534,0.1401304 -1.13866,0.1416967 -1.358195,0.00141 -1.957049,-0.060918 -1.330784,-0.1388498 z m 163.688888,0 c 0.62627,-0.077932 1.73752,-0.079213 2.46945,-0.00282 0.73193,0.076369 0.21953,0.1401304 -1.13866,0.1416967 -1.3582,0.00141 -1.95705,-0.060918 -1.33079,-0.1388498 z M 40.252097,-90.26661 c 0.824618,-0.07427 2.173993,-0.07427 2.998611,0 0.824619,0.07427 0.149931,0.135044 -1.499305,0.135044 -1.649236,0 -2.323924,-0.06077 -1.499306,-0.135044 z m 210.608343,0 c 0.82461,-0.07427 2.17399,-0.07427 2.99861,0 0.82461,0.07427 0.14993,0.135044 -1.49931,0.135044 -1.64924,0 -2.32392,-0.06077 -1.4993,-0.135044 z m -146.40279,-0.328108 c 0.24254,-0.09786 0.63941,-0.09786 0.88195,0 0.24253,0.09786 0.0441,0.177938 -0.44097,0.177938 -0.48507,0 -0.68351,-0.08007 -0.44098,-0.177938 z m 83.49174,-0.242333 c -0.10045,-0.261786 -0.12656,-1.571474 -0.058,-2.910417 l 0.12462,-2.434445 0.0691,2.709492 c 0.0638,2.503438 0.12946,2.721119 0.86284,2.862375 l 0.79375,0.152883 -0.80482,0.04804 c -0.44265,0.02642 -0.88701,-0.166148 -0.98747,-0.42793 z M 106.0779,-126.16802 c 0.001,-1.55222 0.0633,-2.14334 0.13782,-1.3136 0.0745,0.82975 0.0735,2.09975 -0.002,2.82223 -0.0758,0.72247 -0.13677,0.0436 -0.13549,-1.50863 z m 213.44303,-6.87917 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -345.374017,-6.17361 c 9.19e-4,-1.94028 0.06016,-2.68902 0.131648,-1.66387 0.07149,1.02515 0.07074,2.61265 -0.0017,3.52778 -0.07241,0.91513 -0.130896,0.0764 -0.129978,-1.86391 z m 213.433677,-20.10833 c 7.1e-4,-2.13431 0.0589,-2.96195 0.12912,-1.83921 0.0702,1.12275 0.0696,2.869 -0.001,3.88056 -0.071,1.01156 -0.12847,0.093 -0.12767,-2.04135 z m -213.154042,-15.34583 c 0,-0.48507 0.08007,-0.68351 0.177936,-0.44098 0.09787,0.24254 0.09787,0.63941 0,0.88195 -0.09786,0.24253 -0.177936,0.0441 -0.177936,-0.44097 z m 344.636042,-0.55857 c 0.0169,-0.41097 0.10052,-0.49456 0.21314,-0.21314 0.10191,0.25466 0.0894,0.55893 -0.0278,0.67616 -0.11722,0.11723 -0.2006,-0.0911 -0.18528,-0.46302 z M 16.263208,-221.82804 c 0.242535,-0.0979 0.63941,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.485069,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 32.984723,-0.37182 c 0.533576,-0.0808 1.406701,-0.0808 1.940278,0 0.533576,0.0808 0.09701,0.14696 -0.970139,0.14696 -1.067153,0 -1.503716,-0.0661 -0.970139,-0.14696 z m 202.324379,-0.003 c 0.63406,-0.0785 1.58656,-0.0769 2.11667,0.004 0.5301,0.0805 0.0113,0.14473 -1.15285,0.14275 -1.16417,-0.002 -1.59789,-0.0678 -0.96382,-0.14635 z'),
									$elm$svg$Svg$Attributes$style('fill:#b9b9b9;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1101'),
									$elm$svg$Svg$Attributes$d('m 50.663893,123.1691 c 0.536243,-0.0811 1.329993,-0.0789 1.763889,0.005 0.433895,0.0837 -0.0048,0.15008 -0.97499,0.14744 -0.970139,-0.003 -1.325143,-0.0712 -0.788899,-0.15225 z m 184.850707,-0.004 c 0.72761,-0.0761 1.91823,-0.0761 2.64584,0 0.7276,0.0761 0.13229,0.13838 -1.32292,0.13838 -1.45521,0 -2.05052,-0.0623 -1.32292,-0.13838 z M 18.379875,122.83583 c 0.242534,-0.0979 0.639409,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.48507,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 253.294445,0 c 0.24254,-0.0979 0.63941,-0.0979 0.88195,0 0.24253,0.0979 0.0441,0.17793 -0.44097,0.17793 -0.48507,0 -0.68351,-0.0801 -0.44098,-0.17793 z M -25.560381,77.031967 c 0.0072,-0.582083 0.08625,-0.777501 0.175699,-0.434259 0.08944,0.343242 0.08356,0.819492 -0.01307,1.058333 -0.09663,0.238841 -0.169816,-0.04199 -0.162627,-0.624074 z M 187.58059,68.212523 c 7.1e-4,-2.134306 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z m 131.91264,-4.056944 c 0,-1.067153 0.0661,-1.503716 0.14697,-0.970139 0.0808,0.533576 0.0808,1.406701 0,1.940277 -0.0808,0.533577 -0.14697,0.09701 -0.14697,-0.970138 z M -25.854757,40.519469 c 0,-1.843264 0.05946,-2.597327 0.132131,-1.675695 0.07267,0.921632 0.07267,2.429757 0,3.351389 -0.07267,0.921632 -0.132131,0.16757 -0.132131,-1.675694 z m 131.936937,-7.9375 c -10e-4,-1.74625 0.0586,-2.505103 0.13257,-1.686338 0.074,0.818762 0.0749,2.247512 0.002,3.175 -0.0729,0.927485 -0.13344,0.257588 -0.13452,-1.488662 z m 81.83499,-33.16110929 c 0.001,-1.35819441 0.0653,-1.87058991 0.14169,-1.13866081 0.0764,0.73193275 0.0751,1.8431827 -0.003,2.46944436 -0.0779,0.62626514 -0.14041,0.0274108 -0.13885,-1.33078355 z M 58.430797,-8.7763796 c 0.927488,-0.072912 2.356238,-0.07203 3.175,0.00212 0.818765,0.073985 0.05991,0.1336393 -1.686338,0.1325633 -1.74625,-0.00106 -2.416147,-0.061609 -1.488662,-0.1345177 z m 174.085193,0.00141 c 0.82462,-0.07427 2.17399,-0.07427 2.99861,0 0.82462,0.074274 0.14993,0.1350433 -1.4993,0.1350433 -1.64924,0 -2.32393,-0.06077 -1.49931,-0.1350433 z M 269.8164,-49.43886 c 0,-22.410207 0.0423,-31.578019 0.094,-20.372916 0.0517,11.205104 0.0517,29.540728 0,40.745832 -0.0517,11.205103 -0.094,2.037291 -0.094,-20.372916 z M 34.441907,-90.268042 c 0.927488,-0.07291 2.356238,-0.07203 3.175,0.0021 0.818766,0.07398 0.05991,0.133639 -1.686338,0.132563 -1.74625,-0.0011 -2.416146,-0.06161 -1.488662,-0.134518 z m 222.062973,0.0014 c 0.82462,-0.07427 2.17399,-0.07427 2.99861,0 0.82462,0.07427 0.14993,0.135043 -1.4993,0.135043 -1.64924,0 -2.32393,-0.06077 -1.49931,-0.135043 z M 101.28897,-90.61677 c 0.63407,-0.07851 1.58657,-0.07689 2.11667,0.0036 0.5301,0.0805 0.0113,0.144735 -1.15285,0.142755 -1.16416,-0.0021 -1.59788,-0.06784 -0.96382,-0.146353 z m 89.25278,0 c 0.63407,-0.07851 1.58657,-0.07689 2.11667,0.0036 0.5301,0.0805 0.0113,0.144735 -1.15285,0.142755 -1.16416,-0.0021 -1.59788,-0.06784 -0.96382,-0.146353 z m -2.62174,-7.858199 c 0,-1.455208 0.0623,-2.050521 0.13838,-1.322916 0.0761,0.727604 0.0761,1.918229 0,2.645833 -0.0761,0.727604 -0.13838,0.132292 -0.13838,-1.322917 z m 131.61456,-24.165281 c 3.4e-4,-3.68652 0.0526,-5.14672 0.11617,-3.24487 0.0636,1.90185 0.0633,4.9181 -7.1e-4,6.70278 -0.0639,1.78468 -0.11589,0.22862 -0.11555,-3.45791 z m -345.385987,-9.70138 c 0,-2.0373 0.05831,-2.87073 0.129568,-1.85209 0.07126,1.01865 0.07126,2.68552 0,3.70417 -0.07126,1.01865 -0.129568,0.18521 -0.129568,-1.85208 z m 131.933597,0.52916 c -10e-4,-1.74625 0.0586,-2.5051 0.13257,-1.68633 0.074,0.81876 0.0749,2.24751 0.002,3.175 -0.0729,0.92748 -0.13344,0.25758 -0.13452,-1.48867 z m 81.49696,-34.74861 c 0,-2.03729 0.0583,-2.87073 0.12957,-1.85208 0.0713,1.01865 0.0713,2.68552 0,3.70417 -0.0713,1.01864 -0.12957,0.18521 -0.12957,-1.85209 z m -213.152592,-6.70277 c 0,-0.48507 0.08007,-0.68351 0.177936,-0.44098 0.09787,0.24254 0.09787,0.63941 0,0.88195 -0.09786,0.24253 -0.177936,0.0441 -0.177936,-0.44097 z m 344.663892,-0.70556 c 0,-0.48507 0.0801,-0.68351 0.17794,-0.44097 0.0979,0.24253 0.0979,0.63941 0,0.88194 -0.0979,0.24254 -0.17794,0.0441 -0.17794,-0.44097 z M 17.674319,-221.82804 c 0.242535,-0.0979 0.63941,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.485069,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 261.430391,-0.007 c 0.25466,-0.10191 0.55893,-0.0894 0.67616,0.0279 0.11722,0.11722 -0.0911,0.2006 -0.46303,0.18528 -0.41096,-0.0169 -0.49455,-0.10052 -0.21313,-0.21313 z M 52.944231,-222.2018 c 0.626261,-0.0779 1.737511,-0.0792 2.469444,-0.003 0.731929,0.0764 0.219534,0.14013 -1.138661,0.14169 -1.358194,0.002 -1.957049,-0.0609 -1.330783,-0.13885 z m 194.917589,0.002 c 0.53358,-0.0808 1.40671,-0.0808 1.94028,0 0.53358,0.0808 0.097,0.14696 -0.97014,0.14696 -1.06715,0 -1.50371,-0.0661 -0.97014,-0.14696 z'),
									$elm$svg$Svg$Attributes$style('fill:#a3a3a3;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1099'),
									$elm$svg$Svg$Attributes$d('m 54.010431,123.16704 c 0.63059,-0.0782 1.662465,-0.0782 2.293056,0 0.63059,0.0783 0.114652,0.14229 -1.146528,0.14229 -1.261181,0 -1.777118,-0.064 -1.146528,-0.14229 z m 176.036119,-0.003 c 0.82461,-0.0743 2.17399,-0.0743 2.99861,0 0.82462,0.0743 0.14993,0.13504 -1.49931,0.13504 -1.64923,0 -2.32392,-0.0608 -1.4993,-0.13504 z M 19.790986,122.83576 c 0.242534,-0.0979 0.639409,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17794 -0.440972,0.17794 -0.48507,0 -0.683507,-0.0801 -0.440972,-0.17794 z m 250.847054,-0.007 c 0.25466,-0.1019 0.55893,-0.0894 0.67616,0.0278 0.11722,0.11722 -0.0911,0.2006 -0.46302,0.18528 -0.41097,-0.0169 -0.49456,-0.10052 -0.21314,-0.21313 z M -25.573452,75.444467 c 0,-0.485069 0.08007,-0.683507 0.177936,-0.440972 0.09787,0.242534 0.09787,0.639409 0,0.881944 -0.09786,0.242535 -0.177936,0.0441 -0.177936,-0.440972 z M 187.50433,72.622245 c 0,-0.48507 0.0801,-0.683507 0.17794,-0.440972 0.0979,0.242534 0.0979,0.639409 0,0.881944 -0.0979,0.242535 -0.17794,0.0441 -0.17794,-0.440972 z M 319.50328,60.098634 c 10e-4,-1.358194 0.0653,-1.87059 0.1417,-1.13866 0.0764,0.731932 0.0751,1.843182 -0.003,2.469444 -0.0779,0.626265 -0.14042,0.02741 -0.13885,-1.330784 z M 106.08892,39.284747 c 7.1e-4,-2.134306 0.0589,-2.961947 0.12912,-1.839204 0.0702,1.122744 0.0696,2.868994 -0.001,3.880556 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z M -25.847254,33.287525 c 6.97e-4,-2.328334 0.05779,-3.234927 0.126876,-2.014647 0.06909,1.220276 0.06851,3.125276 -0.0013,4.233333 -0.06978,1.108054 -0.126306,0.109647 -0.125608,-2.218686 z M 187.92234,4.3597484 c 0.001,-1.5522222 0.0633,-2.1433401 0.13783,-1.3135927 0.0745,0.8297474 0.0735,2.0997473 -0.002,2.822222 -0.0758,0.7224748 -0.13678,0.043593 -0.1355,-1.5086293 z M 52.59932,-8.7749473 c 0.824618,-0.07427 2.173993,-0.07427 2.998611,0 0.824618,0.074274 0.149931,0.1350433 -1.499306,0.1350433 -1.649236,0 -2.323923,-0.06077 -1.499305,-0.1350433 z m 185.74815,-0.00141 c 0.92749,-0.072912 2.35624,-0.07203 3.175,0.00212 0.81876,0.073984 0.0599,0.1336392 -1.68634,0.1325633 -1.74625,-0.00106 -2.41615,-0.061609 -1.48866,-0.1345177 z M 28.61043,-90.26661 c 0.824618,-0.07427 2.173993,-0.07427 2.998612,0 0.824618,0.07427 0.14993,0.135044 -1.499306,0.135044 -1.649236,0 -2.323924,-0.06077 -1.499306,-0.135044 z m 233.72593,-0.0014 c 0.92748,-0.07291 2.35623,-0.07203 3.175,0.0021 0.81876,0.07398 0.0599,0.133639 -1.68634,0.132563 -1.74625,-0.0011 -2.41615,-0.06161 -1.48866,-0.134518 z M 97.408417,-90.616728 c 0.634065,-0.07851 1.586565,-0.0769 2.116667,0.0036 0.530106,0.0805 0.01132,0.144734 -1.152847,0.142755 -1.164166,-0.0021 -1.597885,-0.06784 -0.96382,-0.146355 z m 97.013893,0 c 0.63406,-0.07851 1.58656,-0.0769 2.11666,0.0036 0.53011,0.0805 0.0113,0.144734 -1.15284,0.142755 -1.16417,-0.0021 -1.59789,-0.06784 -0.96382,-0.146355 z m -6.5023,-12.797132 c 0,-1.45521 0.0623,-2.05052 0.13838,-1.32291 0.0761,0.7276 0.0761,1.91823 0,2.64583 -0.0761,0.7276 -0.13838,0.13229 -0.13838,-1.32292 z m 131.61574,-5.82083 c -3.1e-4,-3.88056 0.0512,-5.51619 0.11454,-3.63474 0.0633,1.88145 0.0636,5.05645 7.1e-4,7.05556 -0.063,1.9991 -0.1148,0.45974 -0.11511,-3.42082 z m -345.38572,-15.875 c 7.96e-4,-2.1343 0.0589,-2.96195 0.129121,-1.8392 0.07022,1.12274 0.06957,2.86899 -0.0014,3.88055 -0.07102,1.01156 -0.128469,0.093 -0.127674,-2.04135 z m 131.93744,-13.22917 c 0,-2.03729 0.0583,-2.87072 0.12957,-1.85208 0.0713,1.01865 0.0713,2.68552 0,3.70417 -0.0713,1.01864 -0.12957,0.18521 -0.12957,-1.85209 z m -131.62766,-32.98472 c 0.0039,-0.77611 0.07616,-1.05207 0.160542,-0.61324 0.08438,0.43882 0.08118,1.07382 -0.0071,1.41111 -0.0883,0.33728 -0.157337,-0.0217 -0.153422,-0.79787 z m 213.07066,0.17639 c 0,-0.67909 0.0726,-0.95691 0.16134,-0.61736 0.0887,0.33955 0.0887,0.89518 0,1.23472 -0.0887,0.33955 -0.16134,0.0617 -0.16134,-0.61736 z m 131.55997,-1.41111 c 0,-0.48507 0.0801,-0.68351 0.17794,-0.44097 0.0979,0.24253 0.0979,0.63941 0,0.88194 -0.0979,0.24254 -0.17794,0.0441 -0.17794,-0.44097 z M 19.08543,-221.82804 c 0.242535,-0.0979 0.63941,-0.0979 0.881945,0 0.242534,0.0979 0.0441,0.17793 -0.440973,0.17793 -0.485069,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 258.41644,-0.0108 c 0.34324,-0.0894 0.81949,-0.0836 1.05833,0.0131 0.23885,0.0966 -0.042,0.16982 -0.62407,0.16263 -0.58208,-0.007 -0.7775,-0.0862 -0.43426,-0.1757 z m -219.423851,-0.36804 c 0.927488,-0.0729 2.356238,-0.072 3.175,0.002 0.818765,0.074 0.05991,0.13364 -1.686338,0.13256 -1.74625,-0.001 -2.416147,-0.0616 -1.488662,-0.13452 z m 186.085961,0.004 c 0.63406,-0.0785 1.58656,-0.0769 2.11666,0.004 0.5301,0.0805 0.0113,0.14473 -1.15284,0.14275 -1.16417,-0.002 -1.59789,-0.0678 -0.96382,-0.14635 z'),
									$elm$svg$Svg$Attributes$style('fill:#8c8c8c;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1097'),
									$elm$svg$Svg$Attributes$d('m 60.184042,123.15957 c 1.697743,-0.0647 4.475868,-0.0647 6.173611,0 1.697744,0.0647 0.308681,0.11759 -3.086805,0.11759 -3.395486,0 -4.784549,-0.0529 -3.086806,-0.11759 z m 160.354258,-6e-5 c 1.60994,-0.0655 4.14994,-0.0652 5.64444,7.1e-4 1.4945,0.066 0.17728,0.11956 -2.92717,0.11912 -3.10444,-3.6e-4 -4.32721,-0.0544 -2.71727,-0.11993 z M 21.378486,122.82628 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339549,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679097,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 246.238894,-0.009 c 0.53358,-0.0808 1.4067,-0.0808 1.94028,0 0.53357,0.0808 0.097,0.14697 -0.97014,0.14697 -1.06715,0 -1.50372,-0.0661 -0.97014,-0.14697 z m 51.47306,-40.669546 c 0,-0.485069 0.0801,-0.683507 0.17794,-0.440972 0.0979,0.242535 0.0979,0.63941 0,0.881945 -0.0979,0.242534 -0.17794,0.0441 -0.17794,-0.440973 z M -25.560381,73.856967 c 0.0072,-0.582083 0.08625,-0.777501 0.175699,-0.434259 0.08944,0.343242 0.08356,0.819492 -0.01307,1.058333 -0.09663,0.238842 -0.169816,-0.04199 -0.162627,-0.624074 z M 319.50328,55.512524 c 10e-4,-1.358195 0.0653,-1.87059 0.1417,-1.138661 0.0764,0.731932 0.0751,1.843182 -0.003,2.469444 -0.0779,0.626265 -0.14042,0.02741 -0.13885,-1.330783 z M 106.09037,46.869468 c 0,-2.231319 0.0573,-3.144131 0.12729,-2.028472 0.07,1.11566 0.07,2.941285 0,4.056945 -0.07,1.115659 -0.12729,0.202847 -0.12729,-2.028473 z M -25.847254,25.173636 c 6.97e-4,-2.328333 0.05779,-3.234926 0.126876,-2.014647 0.06909,1.220276 0.06851,3.125276 -0.0013,4.233334 -0.06978,1.108053 -0.126306,0.109646 -0.125608,-2.218687 z M 187.92001,9.4750259 c 0,-1.4552083 0.0623,-2.0505208 0.13838,-1.3229166 0.0761,0.7276041 0.0761,1.9182287 0,2.6458327 -0.0761,0.727605 -0.13838,0.132292 -0.13838,-1.3229161 z M 42.721542,-8.779103 c 1.600729,-0.065366 4.220104,-0.065366 5.820833,0 1.600729,0.06537 0.291042,0.1188508 -2.910416,0.1188508 -3.201459,0 -4.511146,-0.053481 -2.910417,-0.1188508 z m 202.863978,-2.681e-4 c 1.60994,-0.065518 4.14994,-0.065151 5.64444,7.055e-4 1.49451,0.065962 0.17728,0.119567 -2.92716,0.119119 -3.10445,-3.528e-4 -4.32722,-0.054416 -2.71728,-0.1199303 z m 73.95023,-86.6969869 c -3.1e-4,-3.880555 0.0512,-5.516192 0.11454,-3.634736 0.0633,1.881449 0.0636,5.056448 7.1e-4,7.055555 -0.063,1.999104 -0.1148,0.459737 -0.11511,-3.420819 z m -294.624636,5.214909 c 0.536244,-0.08109 1.329994,-0.07892 1.763889,0.0048 0.433896,0.08374 -0.0048,0.150079 -0.974989,0.147433 -0.970139,-0.0028 -1.325143,-0.07116 -0.7889,-0.152248 z m 242.529876,0.0042 c 0.43656,-0.08409 1.15094,-0.08409 1.5875,0 0.43656,0.08408 0.0794,0.152883 -0.79375,0.152883 -0.87312,0 -1.23031,-0.0688 -0.79375,-0.152883 z M 91.052098,-90.62118 c 1.018646,-0.07126 2.685521,-0.07126 3.704167,0 1.018646,0.07126 0.185208,0.129569 -1.852083,0.129569 -2.037292,0 -2.870729,-0.05831 -1.852084,-0.129569 z m 108.139272,-0.0011 c 1.12274,-0.07022 2.86899,-0.06957 3.88056,0.0014 1.01156,0.07102 0.093,0.128471 -2.04136,0.127674 -2.1343,-7.06e-4 -2.96194,-0.0589 -1.8392,-0.12912 z m -11.26903,-17.90689 c 0.001,-1.55223 0.0633,-2.14334 0.13783,-1.3136 0.0745,0.82975 0.0735,2.09975 -0.002,2.82222 -0.0758,0.72248 -0.13678,0.0436 -0.1355,-1.50862 z m -213.77231,-9.17223 c 7.96e-4,-2.1343 0.0589,-2.96194 0.129121,-1.8392 0.07022,1.12274 0.06957,2.86899 -0.0014,3.88056 -0.07102,1.01156 -0.128469,0.093 -0.127674,-2.04136 z m 131.94034,-28.04583 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -131.62344,-22.93055 c 0,-0.87313 0.0688,-1.23032 0.152883,-0.79375 0.08408,0.43656 0.08408,1.15093 0,1.5875 -0.08409,0.43656 -0.152883,0.0794 -0.152883,-0.79375 z m 344.64966,-2.11667 c 0,-0.6791 0.0726,-0.95691 0.16133,-0.61736 0.0887,0.33955 0.0887,0.89517 0,1.23472 -0.0887,0.33955 -0.16133,0.0617 -0.16133,-0.61736 z m -192.61032,-1.67564 c 11.2051,-0.0517 29.54073,-0.0517 40.74583,0 11.2051,0.0517 2.03729,0.094 -20.37292,0.094 -22.41021,0 -31.57802,-0.0423 -20.37291,-0.094 z m 192.20355,-11.58292 c 0.0169,-0.41097 0.10052,-0.49456 0.21313,-0.21314 0.10191,0.25466 0.0894,0.55893 -0.0278,0.67616 -0.11723,0.11722 -0.20061,-0.0911 -0.18529,-0.46302 z m -297.856388,-37.7909 c 0.438824,-0.0844 1.073824,-0.0812 1.411112,0.007 0.337287,0.0883 -0.02175,0.15733 -0.797864,0.15342 -0.776111,-0.004 -1.052071,-0.0762 -0.613248,-0.16054 z m 254.525058,0.006 c 0.33955,-0.0887 0.89518,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16134 -0.61736,0.16134 -0.67909,0 -0.95691,-0.0726 -0.61736,-0.16134 z M 65.49303,-222.2101 c 1.707272,-0.0648 4.406022,-0.0645 5.997223,7.3e-4 1.591204,0.0652 0.194345,0.11824 -3.104127,0.11784 -3.298473,-4.1e-4 -4.600364,-0.0538 -2.893096,-0.11857 z m 170.19796,9.8e-4 c 1.4067,-0.067 3.70858,-0.067 5.11528,0 1.4067,0.067 0.25576,0.12174 -2.55764,0.12174 -2.8134,0 -3.96434,-0.0548 -2.55764,-0.12174 z'),
									$elm$svg$Svg$Attributes$style('fill:#767676;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1095'),
									$elm$svg$Svg$Attributes$d('m 70.767376,123.16294 c 0.921632,-0.0727 2.429757,-0.0727 3.351389,0 0.921632,0.0727 0.167569,0.13213 -1.675695,0.13213 -1.843264,0 -2.597326,-0.0595 -1.675694,-0.13213 z m 142.698614,10e-4 c 0.82462,-0.0743 2.17399,-0.0743 2.99861,0 0.82462,0.0743 0.14993,0.13505 -1.4993,0.13505 -1.64924,0 -2.32393,-0.0608 -1.49931,-0.13505 z m -190.147226,-0.3281 c 0.242534,-0.0979 0.639409,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.48507,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 241.829176,-0.01 c 0.33954,-0.0887 0.89517,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16133 -0.61736,0.16133 -0.6791,0 -0.95691,-0.0726 -0.61736,-0.16133 z m 53.9425,-42.090197 c 0,-0.485069 0.0801,-0.683507 0.17794,-0.440972 0.0979,0.242535 0.0979,0.63941 0,0.881945 -0.0979,0.242534 -0.17794,0.0441 -0.17794,-0.440973 z M -25.53307,71.563912 c 0,-0.873125 0.0688,-1.230313 0.152883,-0.79375 0.08408,0.436562 0.08408,1.150937 0,1.5875 -0.08409,0.436562 -0.152883,0.07937 -0.152883,-0.79375 z M 106.08892,54.45419 c 7.1e-4,-2.134305 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z m 213.42382,-4.233333 c -0.001,-1.74625 0.0586,-2.505103 0.13257,-1.686338 0.074,0.818762 0.0749,2.247512 0.002,3.175 -0.0731,0.927485 -0.13361,0.257588 -0.13468,-1.488662 z M -25.845986,16.883359 c 0,-2.425347 0.05636,-3.417535 0.12525,-2.204861 0.06889,1.212673 0.06889,3.197048 0,4.409722 -0.06889,1.212673 -0.12525,0.220486 -0.12525,-2.204861 z M 187.92234,14.590303 c 0.001,-1.552222 0.0633,-2.14334 0.13783,-1.313592 0.0745,0.829747 0.0735,2.099747 -0.002,2.822222 -0.0758,0.722475 -0.13678,0.04359 -0.1355,-1.50863 z M 105.63203,-7.6640924 c 0.0169,-0.4109685 0.10052,-0.4945592 0.21314,-0.2131378 0.10191,0.2546597 0.0894,0.558934 -0.0278,0.6761586 -0.11722,0.1172245 -0.2006,-0.091137 -0.18528,-0.4630208 z m -3.28472,-0.7554383 c 0.63406,-0.078514 1.58656,-0.076895 2.11666,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.15284,0.142755 -1.16417,-0.00212 -1.59789,-0.067839 -0.96382,-0.1463533 z m 87.48889,0 c 0.63406,-0.078514 1.58656,-0.076895 2.11666,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.15284,0.142755 -1.16417,-0.00212 -1.59789,-0.067839 -0.96382,-0.1463533 z M 35.665986,-8.7749473 c 0.824618,-0.07427 2.173993,-0.07427 2.998611,0 0.824618,0.074274 0.149931,0.1350433 -1.499305,0.1350433 -1.649237,0 -2.323924,-0.06077 -1.499306,-0.1350433 z m 219.614814,-0.00141 c 0.92749,-0.072912 2.35624,-0.07203 3.175,0.00212 0.81877,0.073984 0.0599,0.1336392 -1.68634,0.1325633 -1.74625,-0.00106 -2.41614,-0.061609 -1.48866,-0.1345177 z m 64.25552,-72.7652777 c 0,-3.977569 0.0514,-5.604757 0.11433,-3.615972 0.0629,1.988784 0.0629,5.243159 0,7.231944 -0.0629,1.988785 -0.11433,0.361597 -0.11433,-3.615972 z M 86.113209,-90.61628 c 0.630591,-0.07825 1.662466,-0.07825 2.293056,0 0.63059,0.07826 0.114653,0.142286 -1.146528,0.142286 -1.26118,0 -1.777118,-0.06403 -1.146528,-0.142286 z m 119.597991,-3.52e-4 c 0.63406,-0.07852 1.58656,-0.0769 2.11666,0.0036 0.53011,0.0805 0.0113,0.144734 -1.15284,0.142755 -1.16417,-0.0021 -1.59789,-0.06784 -0.96382,-0.146353 z m -100.07917,-0.655673 c 0.0169,-0.410969 0.10052,-0.494559 0.21314,-0.213138 0.10191,0.25466 0.0894,0.558934 -0.0278,0.676159 -0.11722,0.117224 -0.2006,-0.09114 -0.18528,-0.463021 z m -131.482,-19.020715 c 7.96e-4,-2.13431 0.0589,-2.96195 0.129121,-1.83921 0.07022,1.12275 0.06957,2.869 -0.0014,3.88056 -0.07102,1.01156 -0.128469,0.093 -0.127674,-2.04135 z m 213.76998,-3.35139 c 0,-1.45521 0.0623,-2.05052 0.13838,-1.32292 0.0761,0.72761 0.0761,1.91823 0,2.64583 -0.0761,0.72761 -0.13838,0.1323 -0.13838,-1.32291 z m -81.82964,-39.86389 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -131.63056,-12.52361 c 0.0039,-0.77611 0.07616,-1.05207 0.160542,-0.61325 0.08438,0.43883 0.08118,1.07383 -0.0071,1.41111 -0.0883,0.33729 -0.157337,-0.0217 -0.153422,-0.79786 z m 344.68065,-1.94028 c 0,-1.06715 0.0661,-1.50371 0.14696,-0.97014 0.0808,0.53358 0.0808,1.40671 0,1.94028 -0.0808,0.53358 -0.14696,0.097 -0.14696,-0.97014 z m -12.14516,-40.83402 c -0.43881,-0.55953 -0.42633,-0.57201 0.13319,-0.1332 0.33955,0.2663 0.61736,0.54411 0.61736,0.61737 0,0.29034 -0.28969,0.10347 -0.75055,-0.48417 z M 23.142375,-221.83759 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339549,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679097,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 250.478945,-0.001 c 0.34324,-0.0894 0.81949,-0.0836 1.05833,0.0131 0.23884,0.0966 -0.042,0.16982 -0.62408,0.16263 -0.58208,-0.007 -0.7775,-0.0863 -0.43425,-0.1757 z m -197.904412,-0.36804 c 0.927488,-0.0729 2.356238,-0.072 3.175,0.002 0.818766,0.074 0.05991,0.13364 -1.686338,0.13256 -1.74625,-0.001 -2.416147,-0.0616 -1.488662,-0.13452 z m 152.049462,-0.001 c 1.12274,-0.0702 2.86899,-0.0696 3.88056,0.001 1.01156,0.071 0.093,0.12847 -2.04136,0.12767 -2.1343,-8e-4 -2.96194,-0.0589 -1.8392,-0.12912 z'),
									$elm$svg$Svg$Attributes$style('fill:#606060;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1093'),
									$elm$svg$Svg$Attributes$d('m 77.117376,123.16294 c 0.921632,-0.0727 2.429757,-0.0727 3.351389,0 0.921632,0.0727 0.167569,0.13213 -1.675695,0.13213 -1.843264,0 -2.597326,-0.0595 -1.675694,-0.13213 z m 130.704174,10e-4 c 0.82461,-0.0743 2.17399,-0.0743 2.99861,0 0.82461,0.0743 0.14993,0.13505 -1.49931,0.13505 -1.64924,0 -2.32392,-0.0608 -1.4993,-0.13505 z m -182.915286,-0.3376 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339548,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679098,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 238.125006,0 c 0.33955,-0.0887 0.89517,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16134 -0.61736,0.16134 -0.6791,0 -0.95691,-0.0726 -0.61736,-0.16134 z m -275.979415,-12.72146 -1.215731,-1.32291 1.322917,1.21573 c 0.727604,0.66865 1.322916,1.26396 1.322916,1.32291 0,0.26939 -0.298526,0.0156 -1.430102,-1.21573 z M 319.06259,79.472014 c 0.0169,-0.410968 0.10052,-0.494559 0.21314,-0.213138 0.10191,0.25466 0.0894,0.558934 -0.0278,0.676159 -0.11722,0.117225 -0.2006,-0.09114 -0.18528,-0.463021 z M -25.53307,68.741689 c 0,-0.873124 0.0688,-1.230312 0.152883,-0.793749 0.08408,0.436562 0.08408,1.150937 0,1.587499 -0.08409,0.436563 -0.152883,0.07937 -0.152883,-0.79375 z m 131.62199,-6.879166 c 7.1e-4,-2.134305 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z M 319.53457,40.695858 c 3.4e-4,-3.686528 0.0526,-5.146721 0.11617,-3.244871 0.0636,1.901849 0.0633,4.918099 -7.1e-4,6.702777 -0.0639,1.784678 -0.11589,0.228621 -0.11555,-3.457906 z M 187.91717,19.529192 c 0.001,-1.358194 0.0653,-1.87059 0.14169,-1.138661 0.0764,0.731933 0.0751,1.843183 -0.003,2.469445 -0.0779,0.626265 -0.14041,0.02741 -0.13885,-1.330784 z M -25.848522,8.7694703 c 0,-2.2313193 0.05728,-3.1441317 0.127289,-2.0284721 0.07001,1.1156597 0.07001,2.9412846 0,4.0569438 -0.07001,1.11566 -0.127289,0.202848 -0.127289,-2.0284717 z M 105.72279,-4.6360845 c 0,-1.4552083 0.0623,-2.0505207 0.13838,-1.3229166 0.0761,0.7276041 0.0761,1.918229 0,2.6458332 -0.0761,0.7276041 -0.13838,0.1322916 -0.13838,-1.3229166 z m -7.25604,-3.7834462 c 0.634065,-0.078514 1.58657,-0.076895 2.11667,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.152849,0.142755 -1.164167,-0.00212 -1.597886,-0.067839 -0.963821,-0.1463533 z m 95.25,0 c 0.63407,-0.078514 1.58657,-0.076895 2.11667,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.15285,0.142755 -1.16416,-0.00212 -1.59788,-0.067839 -0.96382,-0.1463533 z M 29.855796,-8.7763796 c 0.927488,-0.072912 2.356238,-0.07203 3.175,0.00212 0.818765,0.073985 0.05991,0.1336393 -1.686338,0.1325633 -1.74625,-0.00106 -2.416147,-0.061609 -1.488662,-0.1345177 z m 231.235194,0.00141 c 0.82462,-0.07427 2.17399,-0.07427 2.99861,0 0.82462,0.074274 0.14993,0.1350433 -1.4993,0.1350433 -1.64924,0 -2.32393,-0.06077 -1.49931,-0.1350433 z m 58.44358,-59.1847234 c 3.4e-4,-3.686528 0.0526,-5.146721 0.11617,-3.244871 0.0636,1.901849 0.0633,4.918099 -7.1e-4,6.702777 -0.0639,1.784678 -0.11589,0.228621 -0.11555,-3.457906 z M 82.238972,-90.616749 c 0.634065,-0.07851 1.586565,-0.0769 2.116667,0.0036 0.530101,0.0805 0.01132,0.144734 -1.152846,0.142755 -1.164167,-0.0021 -1.597886,-0.06784 -0.963821,-0.146355 z m 127.352778,0 c 0.63407,-0.07851 1.58657,-0.0769 2.11667,0.0036 0.5301,0.0805 0.0113,0.144734 -1.15285,0.142755 -1.16416,-0.0021 -1.59788,-0.06784 -0.96382,-0.146355 z m -103.86896,-3.624886 c 0,-1.455209 0.0623,-2.050521 0.13838,-1.322917 0.0761,0.727604 0.0761,1.918229 0,2.645833 -0.0761,0.727604 -0.13838,0.132292 -0.13838,-1.322916 z m -131.574207,-8.819445 c 0,-2.03729 0.05831,-2.87073 0.129568,-1.85208 0.07126,1.01864 0.07126,2.68552 0,3.70416 -0.07126,1.01865 -0.129568,0.18521 -0.129568,-1.85208 z M 187.92001,-118.5833 c 0,-1.45521 0.0623,-2.05052 0.13838,-1.32292 0.0761,0.72761 0.0761,1.91823 0,2.64584 -0.0761,0.7276 -0.13838,0.13229 -0.13838,-1.32292 z m -81.83254,-42.33333 c 0,-2.03729 0.0583,-2.87073 0.12957,-1.85209 0.0713,1.01865 0.0713,2.68553 0,3.70417 -0.0713,1.01865 -0.12957,0.18521 -0.12957,-1.85208 z m -131.62766,-2.64584 c 0.0039,-0.77611 0.07616,-1.05207 0.160542,-0.61324 0.08438,0.43882 0.08118,1.07382 -0.0071,1.41111 -0.0883,0.33729 -0.157337,-0.0217 -0.153422,-0.79787 z m 344.67101,-1.23472 c 0,-0.87312 0.0688,-1.23031 0.15289,-0.79375 0.0841,0.43657 0.0841,1.15094 0,1.5875 -0.0841,0.43657 -0.15289,0.0794 -0.15289,-0.79375 z m -293.871779,-57.0404 c 0.339549,-0.0887 0.895174,-0.0887 1.234723,0 0.339548,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679098,0 -0.95691,-0.0726 -0.617362,-0.16134 z m 246.768059,0.01 c 0.24254,-0.0979 0.63941,-0.0979 0.88195,0 0.24253,0.0979 0.0441,0.17793 -0.44098,0.17793 -0.48506,0 -0.6835,-0.0801 -0.44097,-0.17793 z m -190.323613,-0.37845 c 0.921632,-0.0727 2.429757,-0.0727 3.351389,0 0.921632,0.0727 0.16757,0.13213 -1.675694,0.13213 -1.843264,0 -2.597327,-0.0595 -1.675695,-0.13213 z m 138.654553,-0.002 c 1.12274,-0.0702 2.86899,-0.0696 3.88055,0.001 1.01156,0.071 0.093,0.12847 -2.04135,0.12767 -2.13431,-8e-4 -2.96195,-0.0589 -1.8392,-0.12912 z'),
									$elm$svg$Svg$Attributes$style('fill:#494949;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1091'),
									$elm$svg$Svg$Attributes$d('m 47.395847,123.07392 c -22.849798,-0.18551 -35.566241,-0.41845 -37.136374,-0.68026 -5.3186904,-0.88686 -11.18045598,-3.28453 -16.3094601,-6.67113 -3.1648662,-2.08971 -8.4897569,-7.22408 -11.1457569,-10.74696 -2.559911,-3.39543 -5.655096,-9.615701 -6.883002,-13.832493 -0.888897,-3.052589 -0.937946,-3.691847 -1.272189,-16.580554 -0.476635,-18.379485 -0.467799,-224.741123 0.0105,-245.180543 0.3602,-15.39267 0.399254,-15.97683 1.285393,-19.22639 2.663154,-9.76605 9.626231,-19.26009 18.5704711,-25.32053 2.7284173,-1.84872 7.8141462,-4.23812 11.1166274,-5.22287 2.9814136,-0.88902 3.7208786,-0.95199 15.1290705,-1.28849 16.512678,-0.48706 243.703033,-0.49202 255.411113,-0.006 7.7731,0.32295 9.17819,0.47309 11.84326,1.26548 12.91038,3.83857 23.6402,13.84271 28.89526,26.94098 1.20996,3.01585 1.46208,4.11828 1.76551,7.71978 0.93333,11.07792 1.25513,251.581245 0.35891,268.234637 -0.19242,3.575547 -0.54539,6.411426 -1.01252,8.134904 -4.05586,14.964219 -16.29086,27.104499 -31.327,31.084449 -2.40865,0.63755 -4.36568,0.76997 -15.28592,1.03429 -17.70748,0.4286 -182.361798,0.67944 -224.013891,0.34127 z M 187.93079,32.68885 c 0.13465,-22.351424 0.33259,-40.7267352 0.43986,-40.8340255 0.10727,-0.1072938 18.47777,-0.2896623 40.82332,-0.4052711 22.34555,-0.1156088 40.66796,-0.2346713 40.71647,-0.2645833 0.0485,-0.029915 0.0882,-18.3641141 0.0882,-40.7426701 v -40.688286 l -40.79011,-0.219418 c -22.43456,-0.120678 -40.84435,-0.273653 -40.91064,-0.33994 -0.0663,-0.06629 -0.23104,-18.487826 -0.36612,-40.936736 l -0.24559,-40.81622 h -40.68822 c -22.37852,0 -40.70841,0.0397 -40.7331,0.0882 -0.0247,0.0485 -0.14375,18.37094 -0.26458,40.71652 -0.12084,22.34558 -0.31861,40.7272 -0.4395,40.848044 -0.12089,0.12084 -18.496207,0.318699 -40.834033,0.439681 l -40.614223,0.219964 v 40.688237 c 0,22.378531 0.03969,40.7135164 0.08819,40.7444127 0.04851,0.030903 18.291553,0.1499588 40.540101,0.2645833 22.248552,0.1146246 40.584185,0.3406563 40.745835,0.502292 0.16166,0.1616393 0.39229,18.497263 0.5125,40.745831 0.12021,22.248572 0.23927,40.491634 0.26458,40.540141 0.0253,0.04851 18.35567,0.08819 40.73413,0.08819 h 40.68812 z')
								]),
							_List_Nil)
						]))
				]))
		]));
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $author$project$Icons$mana = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$id('svg8'),
			$elm$svg$Svg$Attributes$version('1.1'),
			$elm$svg$Svg$Attributes$viewBox('0 0 513.10925 482.95688'),
			$elm$svg$Svg$Attributes$height('64'),
			$elm$svg$Svg$Attributes$width('64')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$defs,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('defs2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$metadata,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('metadata5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$transform('translate(76.185345,155.19473)'),
					$elm$svg$Svg$Attributes$id('layer1')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$transform('translate(6.2060812,127.18329)'),
							$elm$svg$Svg$Attributes$id('g1089')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1105'),
									$elm$svg$Svg$Attributes$d('m 41.850241,123.1625 c 0.927488,-0.0729 2.356238,-0.072 3.175,0.002 0.818765,0.074 0.05991,0.13364 -1.686338,0.13256 -1.74625,-0.001 -2.416147,-0.0616 -1.488662,-0.13452 z m 202.141669,0 c 0.92749,-0.0729 2.35624,-0.072 3.175,0.002 0.81877,0.074 0.0599,0.13364 -1.68634,0.13256 -1.74625,-0.001 -2.41614,-0.0616 -1.48866,-0.13452 z M 15.028486,122.82628 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339548,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679098,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 259.291674,0 c 0.33955,-0.0887 0.89517,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16134 -0.61736,0.16134 -0.6791,0 -0.95691,-0.0726 -0.61736,-0.16134 z m 30.95625,-11.1789 c 0,-0.0733 0.27781,-0.35107 0.61736,-0.61736 0.55953,-0.43881 0.572,-0.42633 0.13319,0.1332 -0.46085,0.58763 -0.75055,0.77451 -0.75055,0.48416 z M -25.573452,80.383356 c 0,-0.48507 0.08007,-0.683507 0.177936,-0.440973 0.09787,0.242535 0.09787,0.63941 0,0.881945 -0.09786,0.242534 -0.177936,0.0441 -0.177936,-0.440972 z M 319.47648,69.623634 c 0.004,-0.776111 0.0762,-1.052072 0.16055,-0.613248 0.0844,0.438824 0.0812,1.073824 -0.007,1.411111 -0.0883,0.337288 -0.15734,-0.02177 -0.15342,-0.797863 z M 187.58059,53.043079 c 7.1e-4,-2.134305 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z M -25.866171,50.573635 c 0.0016,-1.358195 0.06533,-1.87059 0.141696,-1.138661 0.07637,0.731933 0.07509,1.843183 -0.0028,2.469444 -0.07793,0.626266 -0.140415,0.02741 -0.13885,-1.330783 z M 106.0779,21.998636 c 0.001,-1.552222 0.0633,-2.14334 0.13782,-1.313592 0.0745,0.829747 0.0735,2.099747 -0.002,2.822222 -0.0758,0.722474 -0.13677,0.04359 -0.13549,-1.50863 z M 69.369145,-8.7777236 c 1.122743,-0.07022 2.868993,-0.069568 3.880555,0.00141 1.011562,0.071018 0.09295,0.128471 -2.041352,0.1276738 -2.134306,-7.056e-4 -2.961947,-0.0589 -1.839203,-0.1291202 z m 151.694445,0 c 1.12275,-0.07022 2.869,-0.069568 3.88056,0.00141 1.01156,0.071018 0.093,0.128471 -2.04135,0.1276738 -2.13431,-7.056e-4 -2.96195,-0.0589 -1.83921,-0.1291202 z M 47.307653,-90.270765 c 1.600729,-0.06537 4.220104,-0.06537 5.820833,0 1.60073,0.06537 0.291042,0.11885 -2.910416,0.11885 -3.201459,0 -4.511146,-0.05348 -2.910417,-0.11885 z m 193.675007,0 c 1.60073,-0.06537 4.2201,-0.06537 5.82083,0 1.60073,0.06537 0.29104,0.11885 -2.91042,0.11885 -3.20145,0 -4.51114,-0.05348 -2.91041,-0.11885 z M 106.07557,-121.05275 c 0,-1.4552 0.0623,-2.05052 0.13838,-1.32291 0.0761,0.7276 0.0761,1.91823 0,2.64583 -0.0761,0.72761 -0.13838,0.13229 -0.13838,-1.32292 z m 213.4408,-19.22638 c 0.001,-1.94028 0.0602,-2.68902 0.13164,-1.66387 0.0715,1.02515 0.0707,2.61265 -0.002,3.52778 -0.0723,0.91512 -0.1308,0.0764 -0.12988,-1.86391 z m -345.388987,-4.23334 c 0.002,-1.16416 0.06784,-1.59788 0.146354,-0.96382 0.07851,0.63407 0.0769,1.58657 -0.0036,2.11667 -0.08049,0.5301 -0.144734,0.0113 -0.142754,-1.15285 z m 213.454657,-7.23194 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -213.142421,-24.51805 c 0.0072,-0.58209 0.08625,-0.77751 0.175699,-0.43426 0.08944,0.34324 0.08356,0.81949 -0.01307,1.05833 -0.09663,0.23884 -0.169816,-0.042 -0.162627,-0.62407 z m 344.650821,-0.17639 c 0,-0.48507 0.0801,-0.68351 0.17794,-0.44098 0.0979,0.24254 0.0979,0.63941 0,0.88195 -0.0979,0.24253 -0.17794,0.0441 -0.17794,-0.44097 z M 14.32293,-221.83759 c 0.339549,-0.0887 0.895174,-0.0887 1.234722,0 0.339549,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679097,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 266.54567,0.002 c 0.25466,-0.10191 0.55893,-0.0894 0.67615,0.0279 0.11723,0.11722 -0.0911,0.2006 -0.46302,0.18528 -0.41096,-0.0169 -0.49456,-0.10052 -0.21313,-0.21313 z m -237.429678,-0.37229 c 1.025152,-0.0715 2.612652,-0.0707 3.527778,0.002 0.915127,0.0724 0.07637,0.1309 -1.863908,0.12998 -1.940278,-9.2e-4 -2.689021,-0.0602 -1.66387,-0.13165 z m 212.369728,0.002 c 0.82975,-0.0745 2.09975,-0.0735 2.82222,0.002 0.72248,0.0758 0.0436,0.13678 -1.50863,0.1355 -1.55222,-0.001 -2.14334,-0.0633 -1.31359,-0.13783 z'),
									$elm$svg$Svg$Attributes$style('fill:#d0d0d0;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1103'),
									$elm$svg$Svg$Attributes$d('m 47.131264,123.16957 c 0.533576,-0.0808 1.406702,-0.0808 1.940278,0 0.533576,0.0808 0.09701,0.14696 -0.970139,0.14696 -1.067153,0 -1.503715,-0.0661 -0.970139,-0.14696 z m 192.974296,-3.6e-4 c 0.53625,-0.0811 1.33,-0.0789 1.76389,0.005 0.4339,0.0837 -0.005,0.15008 -0.97499,0.14743 -0.97014,-0.003 -1.32514,-0.0712 -0.7889,-0.15225 z M 16.968763,122.83583 c 0.242535,-0.0979 0.63941,-0.0979 0.881945,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.48507,0 -0.683507,-0.0801 -0.440973,-0.17793 z m 256.138717,-0.007 c 0.25466,-0.10191 0.55894,-0.0894 0.67616,0.0278 0.11723,0.11723 -0.0911,0.20061 -0.46302,0.18529 -0.41097,-0.0169 -0.49456,-0.10052 -0.21314,-0.21314 z m 33.22726,-12.23944 c 0,-0.0733 0.27781,-0.35107 0.61736,-0.61736 0.55953,-0.43881 0.57201,-0.42633 0.1332,0.13319 -0.46086,0.58764 -0.75056,0.77452 -0.75056,0.48417 z M -25.560381,78.795856 c 0.0072,-0.582084 0.08625,-0.777501 0.175699,-0.434259 0.08944,0.343242 0.08356,0.819492 -0.01307,1.058333 -0.09663,0.238841 -0.169816,-0.04199 -0.162627,-0.624074 z M 319.47648,67.15419 c 0.004,-0.776111 0.0762,-1.052072 0.16055,-0.613248 0.0844,0.438824 0.0812,1.073824 -0.007,1.411111 -0.0883,0.337287 -0.15734,-0.02177 -0.15342,-0.797863 z M 187.58204,60.627801 c 0,-2.231319 0.0573,-3.144132 0.12729,-2.028472 0.07,1.11566 0.07,2.941284 0,4.056944 -0.07,1.11566 -0.12729,0.202847 -0.12729,-2.028472 z M -25.866171,45.987524 c 0.0016,-1.358194 0.06533,-1.87059 0.141696,-1.138661 0.07637,0.731933 0.07509,1.843183 -0.0028,2.469445 -0.07793,0.626265 -0.140415,0.02741 -0.13885,-1.330784 z M 106.07557,27.113914 c 0,-1.455208 0.0623,-2.050521 0.13838,-1.322917 0.0761,0.727604 0.0761,1.918229 0,2.645833 -0.0761,0.727605 -0.13838,0.132292 -0.13838,-1.322916 z m 81.84677,-32.6319429 c 0.001,-1.5522222 0.0633,-2.1433401 0.13783,-1.3135927 0.0745,0.8297474 0.0735,2.0997473 -0.002,2.8222221 -0.0758,0.7224747 -0.13678,0.043593 -0.1355,-1.5086294 z M 63.880342,-8.7712502 c 0.626262,-0.077932 1.737512,-0.079213 2.469444,-0.00282 0.73193,0.076369 0.219534,0.1401304 -1.13866,0.1416967 -1.358195,0.00141 -1.957049,-0.060918 -1.330784,-0.1388498 z m 163.688888,0 c 0.62627,-0.077932 1.73752,-0.079213 2.46945,-0.00282 0.73193,0.076369 0.21953,0.1401304 -1.13866,0.1416967 -1.3582,0.00141 -1.95705,-0.060918 -1.33079,-0.1388498 z M 40.252097,-90.26661 c 0.824618,-0.07427 2.173993,-0.07427 2.998611,0 0.824619,0.07427 0.149931,0.135044 -1.499305,0.135044 -1.649236,0 -2.323924,-0.06077 -1.499306,-0.135044 z m 210.608343,0 c 0.82461,-0.07427 2.17399,-0.07427 2.99861,0 0.82461,0.07427 0.14993,0.135044 -1.49931,0.135044 -1.64924,0 -2.32392,-0.06077 -1.4993,-0.135044 z m -146.40279,-0.328108 c 0.24254,-0.09786 0.63941,-0.09786 0.88195,0 0.24253,0.09786 0.0441,0.177938 -0.44097,0.177938 -0.48507,0 -0.68351,-0.08007 -0.44098,-0.177938 z m 83.49174,-0.242333 c -0.10045,-0.261786 -0.12656,-1.571474 -0.058,-2.910417 l 0.12462,-2.434445 0.0691,2.709492 c 0.0638,2.503438 0.12946,2.721119 0.86284,2.862375 l 0.79375,0.152883 -0.80482,0.04804 c -0.44265,0.02642 -0.88701,-0.166148 -0.98747,-0.42793 z M 106.0779,-126.16802 c 0.001,-1.55222 0.0633,-2.14334 0.13782,-1.3136 0.0745,0.82975 0.0735,2.09975 -0.002,2.82223 -0.0758,0.72247 -0.13677,0.0436 -0.13549,-1.50863 z m 213.44303,-6.87917 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -345.374017,-6.17361 c 9.19e-4,-1.94028 0.06016,-2.68902 0.131648,-1.66387 0.07149,1.02515 0.07074,2.61265 -0.0017,3.52778 -0.07241,0.91513 -0.130896,0.0764 -0.129978,-1.86391 z m 213.433677,-20.10833 c 7.1e-4,-2.13431 0.0589,-2.96195 0.12912,-1.83921 0.0702,1.12275 0.0696,2.869 -0.001,3.88056 -0.071,1.01156 -0.12847,0.093 -0.12767,-2.04135 z m -213.154042,-15.34583 c 0,-0.48507 0.08007,-0.68351 0.177936,-0.44098 0.09787,0.24254 0.09787,0.63941 0,0.88195 -0.09786,0.24253 -0.177936,0.0441 -0.177936,-0.44097 z m 344.636042,-0.55857 c 0.0169,-0.41097 0.10052,-0.49456 0.21314,-0.21314 0.10191,0.25466 0.0894,0.55893 -0.0278,0.67616 -0.11722,0.11723 -0.2006,-0.0911 -0.18528,-0.46302 z M 16.263208,-221.82804 c 0.242535,-0.0979 0.63941,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.485069,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 32.984723,-0.37182 c 0.533576,-0.0808 1.406701,-0.0808 1.940278,0 0.533576,0.0808 0.09701,0.14696 -0.970139,0.14696 -1.067153,0 -1.503716,-0.0661 -0.970139,-0.14696 z m 202.324379,-0.003 c 0.63406,-0.0785 1.58656,-0.0769 2.11667,0.004 0.5301,0.0805 0.0113,0.14473 -1.15285,0.14275 -1.16417,-0.002 -1.59789,-0.0678 -0.96382,-0.14635 z'),
									$elm$svg$Svg$Attributes$style('fill:#b9b9b9;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1101'),
									$elm$svg$Svg$Attributes$d('m 50.663893,123.1691 c 0.536243,-0.0811 1.329993,-0.0789 1.763889,0.005 0.433895,0.0837 -0.0048,0.15008 -0.97499,0.14744 -0.970139,-0.003 -1.325143,-0.0712 -0.788899,-0.15225 z m 184.850707,-0.004 c 0.72761,-0.0761 1.91823,-0.0761 2.64584,0 0.7276,0.0761 0.13229,0.13838 -1.32292,0.13838 -1.45521,0 -2.05052,-0.0623 -1.32292,-0.13838 z M 18.379875,122.83583 c 0.242534,-0.0979 0.639409,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.48507,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 253.294445,0 c 0.24254,-0.0979 0.63941,-0.0979 0.88195,0 0.24253,0.0979 0.0441,0.17793 -0.44097,0.17793 -0.48507,0 -0.68351,-0.0801 -0.44098,-0.17793 z M -25.560381,77.031967 c 0.0072,-0.582083 0.08625,-0.777501 0.175699,-0.434259 0.08944,0.343242 0.08356,0.819492 -0.01307,1.058333 -0.09663,0.238841 -0.169816,-0.04199 -0.162627,-0.624074 z M 187.58059,68.212523 c 7.1e-4,-2.134306 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z m 131.91264,-4.056944 c 0,-1.067153 0.0661,-1.503716 0.14697,-0.970139 0.0808,0.533576 0.0808,1.406701 0,1.940277 -0.0808,0.533577 -0.14697,0.09701 -0.14697,-0.970138 z M -25.854757,40.519469 c 0,-1.843264 0.05946,-2.597327 0.132131,-1.675695 0.07267,0.921632 0.07267,2.429757 0,3.351389 -0.07267,0.921632 -0.132131,0.16757 -0.132131,-1.675694 z m 131.936937,-7.9375 c -10e-4,-1.74625 0.0586,-2.505103 0.13257,-1.686338 0.074,0.818762 0.0749,2.247512 0.002,3.175 -0.0729,0.927485 -0.13344,0.257588 -0.13452,-1.488662 z m 81.83499,-33.16110929 c 0.001,-1.35819441 0.0653,-1.87058991 0.14169,-1.13866081 0.0764,0.73193275 0.0751,1.8431827 -0.003,2.46944436 -0.0779,0.62626514 -0.14041,0.0274108 -0.13885,-1.33078355 z M 58.430797,-8.7763796 c 0.927488,-0.072912 2.356238,-0.07203 3.175,0.00212 0.818765,0.073985 0.05991,0.1336393 -1.686338,0.1325633 -1.74625,-0.00106 -2.416147,-0.061609 -1.488662,-0.1345177 z m 174.085193,0.00141 c 0.82462,-0.07427 2.17399,-0.07427 2.99861,0 0.82462,0.074274 0.14993,0.1350433 -1.4993,0.1350433 -1.64924,0 -2.32393,-0.06077 -1.49931,-0.1350433 z M 269.8164,-49.43886 c 0,-22.410207 0.0423,-31.578019 0.094,-20.372916 0.0517,11.205104 0.0517,29.540728 0,40.745832 -0.0517,11.205103 -0.094,2.037291 -0.094,-20.372916 z M 34.441907,-90.268042 c 0.927488,-0.07291 2.356238,-0.07203 3.175,0.0021 0.818766,0.07398 0.05991,0.133639 -1.686338,0.132563 -1.74625,-0.0011 -2.416146,-0.06161 -1.488662,-0.134518 z m 222.062973,0.0014 c 0.82462,-0.07427 2.17399,-0.07427 2.99861,0 0.82462,0.07427 0.14993,0.135043 -1.4993,0.135043 -1.64924,0 -2.32393,-0.06077 -1.49931,-0.135043 z M 101.28897,-90.61677 c 0.63407,-0.07851 1.58657,-0.07689 2.11667,0.0036 0.5301,0.0805 0.0113,0.144735 -1.15285,0.142755 -1.16416,-0.0021 -1.59788,-0.06784 -0.96382,-0.146353 z m 89.25278,0 c 0.63407,-0.07851 1.58657,-0.07689 2.11667,0.0036 0.5301,0.0805 0.0113,0.144735 -1.15285,0.142755 -1.16416,-0.0021 -1.59788,-0.06784 -0.96382,-0.146353 z m -2.62174,-7.858199 c 0,-1.455208 0.0623,-2.050521 0.13838,-1.322916 0.0761,0.727604 0.0761,1.918229 0,2.645833 -0.0761,0.727604 -0.13838,0.132292 -0.13838,-1.322917 z m 131.61456,-24.165281 c 3.4e-4,-3.68652 0.0526,-5.14672 0.11617,-3.24487 0.0636,1.90185 0.0633,4.9181 -7.1e-4,6.70278 -0.0639,1.78468 -0.11589,0.22862 -0.11555,-3.45791 z m -345.385987,-9.70138 c 0,-2.0373 0.05831,-2.87073 0.129568,-1.85209 0.07126,1.01865 0.07126,2.68552 0,3.70417 -0.07126,1.01865 -0.129568,0.18521 -0.129568,-1.85208 z m 131.933597,0.52916 c -10e-4,-1.74625 0.0586,-2.5051 0.13257,-1.68633 0.074,0.81876 0.0749,2.24751 0.002,3.175 -0.0729,0.92748 -0.13344,0.25758 -0.13452,-1.48867 z m 81.49696,-34.74861 c 0,-2.03729 0.0583,-2.87073 0.12957,-1.85208 0.0713,1.01865 0.0713,2.68552 0,3.70417 -0.0713,1.01864 -0.12957,0.18521 -0.12957,-1.85209 z m -213.152592,-6.70277 c 0,-0.48507 0.08007,-0.68351 0.177936,-0.44098 0.09787,0.24254 0.09787,0.63941 0,0.88195 -0.09786,0.24253 -0.177936,0.0441 -0.177936,-0.44097 z m 344.663892,-0.70556 c 0,-0.48507 0.0801,-0.68351 0.17794,-0.44097 0.0979,0.24253 0.0979,0.63941 0,0.88194 -0.0979,0.24254 -0.17794,0.0441 -0.17794,-0.44097 z M 17.674319,-221.82804 c 0.242535,-0.0979 0.63941,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.485069,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 261.430391,-0.007 c 0.25466,-0.10191 0.55893,-0.0894 0.67616,0.0279 0.11722,0.11722 -0.0911,0.2006 -0.46303,0.18528 -0.41096,-0.0169 -0.49455,-0.10052 -0.21313,-0.21313 z M 52.944231,-222.2018 c 0.626261,-0.0779 1.737511,-0.0792 2.469444,-0.003 0.731929,0.0764 0.219534,0.14013 -1.138661,0.14169 -1.358194,0.002 -1.957049,-0.0609 -1.330783,-0.13885 z m 194.917589,0.002 c 0.53358,-0.0808 1.40671,-0.0808 1.94028,0 0.53358,0.0808 0.097,0.14696 -0.97014,0.14696 -1.06715,0 -1.50371,-0.0661 -0.97014,-0.14696 z'),
									$elm$svg$Svg$Attributes$style('fill:#a3a3a3;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1099'),
									$elm$svg$Svg$Attributes$d('m 54.010431,123.16704 c 0.63059,-0.0782 1.662465,-0.0782 2.293056,0 0.63059,0.0783 0.114652,0.14229 -1.146528,0.14229 -1.261181,0 -1.777118,-0.064 -1.146528,-0.14229 z m 176.036119,-0.003 c 0.82461,-0.0743 2.17399,-0.0743 2.99861,0 0.82462,0.0743 0.14993,0.13504 -1.49931,0.13504 -1.64923,0 -2.32392,-0.0608 -1.4993,-0.13504 z M 19.790986,122.83576 c 0.242534,-0.0979 0.639409,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17794 -0.440972,0.17794 -0.48507,0 -0.683507,-0.0801 -0.440972,-0.17794 z m 250.847054,-0.007 c 0.25466,-0.1019 0.55893,-0.0894 0.67616,0.0278 0.11722,0.11722 -0.0911,0.2006 -0.46302,0.18528 -0.41097,-0.0169 -0.49456,-0.10052 -0.21314,-0.21313 z M -25.573452,75.444467 c 0,-0.485069 0.08007,-0.683507 0.177936,-0.440972 0.09787,0.242534 0.09787,0.639409 0,0.881944 -0.09786,0.242535 -0.177936,0.0441 -0.177936,-0.440972 z M 187.50433,72.622245 c 0,-0.48507 0.0801,-0.683507 0.17794,-0.440972 0.0979,0.242534 0.0979,0.639409 0,0.881944 -0.0979,0.242535 -0.17794,0.0441 -0.17794,-0.440972 z M 319.50328,60.098634 c 10e-4,-1.358194 0.0653,-1.87059 0.1417,-1.13866 0.0764,0.731932 0.0751,1.843182 -0.003,2.469444 -0.0779,0.626265 -0.14042,0.02741 -0.13885,-1.330784 z M 106.08892,39.284747 c 7.1e-4,-2.134306 0.0589,-2.961947 0.12912,-1.839204 0.0702,1.122744 0.0696,2.868994 -0.001,3.880556 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z M -25.847254,33.287525 c 6.97e-4,-2.328334 0.05779,-3.234927 0.126876,-2.014647 0.06909,1.220276 0.06851,3.125276 -0.0013,4.233333 -0.06978,1.108054 -0.126306,0.109647 -0.125608,-2.218686 z M 187.92234,4.3597484 c 0.001,-1.5522222 0.0633,-2.1433401 0.13783,-1.3135927 0.0745,0.8297474 0.0735,2.0997473 -0.002,2.822222 -0.0758,0.7224748 -0.13678,0.043593 -0.1355,-1.5086293 z M 52.59932,-8.7749473 c 0.824618,-0.07427 2.173993,-0.07427 2.998611,0 0.824618,0.074274 0.149931,0.1350433 -1.499306,0.1350433 -1.649236,0 -2.323923,-0.06077 -1.499305,-0.1350433 z m 185.74815,-0.00141 c 0.92749,-0.072912 2.35624,-0.07203 3.175,0.00212 0.81876,0.073984 0.0599,0.1336392 -1.68634,0.1325633 -1.74625,-0.00106 -2.41615,-0.061609 -1.48866,-0.1345177 z M 28.61043,-90.26661 c 0.824618,-0.07427 2.173993,-0.07427 2.998612,0 0.824618,0.07427 0.14993,0.135044 -1.499306,0.135044 -1.649236,0 -2.323924,-0.06077 -1.499306,-0.135044 z m 233.72593,-0.0014 c 0.92748,-0.07291 2.35623,-0.07203 3.175,0.0021 0.81876,0.07398 0.0599,0.133639 -1.68634,0.132563 -1.74625,-0.0011 -2.41615,-0.06161 -1.48866,-0.134518 z M 97.408417,-90.616728 c 0.634065,-0.07851 1.586565,-0.0769 2.116667,0.0036 0.530106,0.0805 0.01132,0.144734 -1.152847,0.142755 -1.164166,-0.0021 -1.597885,-0.06784 -0.96382,-0.146355 z m 97.013893,0 c 0.63406,-0.07851 1.58656,-0.0769 2.11666,0.0036 0.53011,0.0805 0.0113,0.144734 -1.15284,0.142755 -1.16417,-0.0021 -1.59789,-0.06784 -0.96382,-0.146355 z m -6.5023,-12.797132 c 0,-1.45521 0.0623,-2.05052 0.13838,-1.32291 0.0761,0.7276 0.0761,1.91823 0,2.64583 -0.0761,0.7276 -0.13838,0.13229 -0.13838,-1.32292 z m 131.61574,-5.82083 c -3.1e-4,-3.88056 0.0512,-5.51619 0.11454,-3.63474 0.0633,1.88145 0.0636,5.05645 7.1e-4,7.05556 -0.063,1.9991 -0.1148,0.45974 -0.11511,-3.42082 z m -345.38572,-15.875 c 7.96e-4,-2.1343 0.0589,-2.96195 0.129121,-1.8392 0.07022,1.12274 0.06957,2.86899 -0.0014,3.88055 -0.07102,1.01156 -0.128469,0.093 -0.127674,-2.04135 z m 131.93744,-13.22917 c 0,-2.03729 0.0583,-2.87072 0.12957,-1.85208 0.0713,1.01865 0.0713,2.68552 0,3.70417 -0.0713,1.01864 -0.12957,0.18521 -0.12957,-1.85209 z m -131.62766,-32.98472 c 0.0039,-0.77611 0.07616,-1.05207 0.160542,-0.61324 0.08438,0.43882 0.08118,1.07382 -0.0071,1.41111 -0.0883,0.33728 -0.157337,-0.0217 -0.153422,-0.79787 z m 213.07066,0.17639 c 0,-0.67909 0.0726,-0.95691 0.16134,-0.61736 0.0887,0.33955 0.0887,0.89518 0,1.23472 -0.0887,0.33955 -0.16134,0.0617 -0.16134,-0.61736 z m 131.55997,-1.41111 c 0,-0.48507 0.0801,-0.68351 0.17794,-0.44097 0.0979,0.24253 0.0979,0.63941 0,0.88194 -0.0979,0.24254 -0.17794,0.0441 -0.17794,-0.44097 z M 19.08543,-221.82804 c 0.242535,-0.0979 0.63941,-0.0979 0.881945,0 0.242534,0.0979 0.0441,0.17793 -0.440973,0.17793 -0.485069,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 258.41644,-0.0108 c 0.34324,-0.0894 0.81949,-0.0836 1.05833,0.0131 0.23885,0.0966 -0.042,0.16982 -0.62407,0.16263 -0.58208,-0.007 -0.7775,-0.0862 -0.43426,-0.1757 z m -219.423851,-0.36804 c 0.927488,-0.0729 2.356238,-0.072 3.175,0.002 0.818765,0.074 0.05991,0.13364 -1.686338,0.13256 -1.74625,-0.001 -2.416147,-0.0616 -1.488662,-0.13452 z m 186.085961,0.004 c 0.63406,-0.0785 1.58656,-0.0769 2.11666,0.004 0.5301,0.0805 0.0113,0.14473 -1.15284,0.14275 -1.16417,-0.002 -1.59789,-0.0678 -0.96382,-0.14635 z'),
									$elm$svg$Svg$Attributes$style('fill:#8c8c8c;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1097'),
									$elm$svg$Svg$Attributes$d('m 60.184042,123.15957 c 1.697743,-0.0647 4.475868,-0.0647 6.173611,0 1.697744,0.0647 0.308681,0.11759 -3.086805,0.11759 -3.395486,0 -4.784549,-0.0529 -3.086806,-0.11759 z m 160.354258,-6e-5 c 1.60994,-0.0655 4.14994,-0.0652 5.64444,7.1e-4 1.4945,0.066 0.17728,0.11956 -2.92717,0.11912 -3.10444,-3.6e-4 -4.32721,-0.0544 -2.71727,-0.11993 z M 21.378486,122.82628 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339549,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679097,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 246.238894,-0.009 c 0.53358,-0.0808 1.4067,-0.0808 1.94028,0 0.53357,0.0808 0.097,0.14697 -0.97014,0.14697 -1.06715,0 -1.50372,-0.0661 -0.97014,-0.14697 z m 51.47306,-40.669546 c 0,-0.485069 0.0801,-0.683507 0.17794,-0.440972 0.0979,0.242535 0.0979,0.63941 0,0.881945 -0.0979,0.242534 -0.17794,0.0441 -0.17794,-0.440973 z M -25.560381,73.856967 c 0.0072,-0.582083 0.08625,-0.777501 0.175699,-0.434259 0.08944,0.343242 0.08356,0.819492 -0.01307,1.058333 -0.09663,0.238842 -0.169816,-0.04199 -0.162627,-0.624074 z M 319.50328,55.512524 c 10e-4,-1.358195 0.0653,-1.87059 0.1417,-1.138661 0.0764,0.731932 0.0751,1.843182 -0.003,2.469444 -0.0779,0.626265 -0.14042,0.02741 -0.13885,-1.330783 z M 106.09037,46.869468 c 0,-2.231319 0.0573,-3.144131 0.12729,-2.028472 0.07,1.11566 0.07,2.941285 0,4.056945 -0.07,1.115659 -0.12729,0.202847 -0.12729,-2.028473 z M -25.847254,25.173636 c 6.97e-4,-2.328333 0.05779,-3.234926 0.126876,-2.014647 0.06909,1.220276 0.06851,3.125276 -0.0013,4.233334 -0.06978,1.108053 -0.126306,0.109646 -0.125608,-2.218687 z M 187.92001,9.4750259 c 0,-1.4552083 0.0623,-2.0505208 0.13838,-1.3229166 0.0761,0.7276041 0.0761,1.9182287 0,2.6458327 -0.0761,0.727605 -0.13838,0.132292 -0.13838,-1.3229161 z M 42.721542,-8.779103 c 1.600729,-0.065366 4.220104,-0.065366 5.820833,0 1.600729,0.06537 0.291042,0.1188508 -2.910416,0.1188508 -3.201459,0 -4.511146,-0.053481 -2.910417,-0.1188508 z m 202.863978,-2.681e-4 c 1.60994,-0.065518 4.14994,-0.065151 5.64444,7.055e-4 1.49451,0.065962 0.17728,0.119567 -2.92716,0.119119 -3.10445,-3.528e-4 -4.32722,-0.054416 -2.71728,-0.1199303 z m 73.95023,-86.6969869 c -3.1e-4,-3.880555 0.0512,-5.516192 0.11454,-3.634736 0.0633,1.881449 0.0636,5.056448 7.1e-4,7.055555 -0.063,1.999104 -0.1148,0.459737 -0.11511,-3.420819 z m -294.624636,5.214909 c 0.536244,-0.08109 1.329994,-0.07892 1.763889,0.0048 0.433896,0.08374 -0.0048,0.150079 -0.974989,0.147433 -0.970139,-0.0028 -1.325143,-0.07116 -0.7889,-0.152248 z m 242.529876,0.0042 c 0.43656,-0.08409 1.15094,-0.08409 1.5875,0 0.43656,0.08408 0.0794,0.152883 -0.79375,0.152883 -0.87312,0 -1.23031,-0.0688 -0.79375,-0.152883 z M 91.052098,-90.62118 c 1.018646,-0.07126 2.685521,-0.07126 3.704167,0 1.018646,0.07126 0.185208,0.129569 -1.852083,0.129569 -2.037292,0 -2.870729,-0.05831 -1.852084,-0.129569 z m 108.139272,-0.0011 c 1.12274,-0.07022 2.86899,-0.06957 3.88056,0.0014 1.01156,0.07102 0.093,0.128471 -2.04136,0.127674 -2.1343,-7.06e-4 -2.96194,-0.0589 -1.8392,-0.12912 z m -11.26903,-17.90689 c 0.001,-1.55223 0.0633,-2.14334 0.13783,-1.3136 0.0745,0.82975 0.0735,2.09975 -0.002,2.82222 -0.0758,0.72248 -0.13678,0.0436 -0.1355,-1.50862 z m -213.77231,-9.17223 c 7.96e-4,-2.1343 0.0589,-2.96194 0.129121,-1.8392 0.07022,1.12274 0.06957,2.86899 -0.0014,3.88056 -0.07102,1.01156 -0.128469,0.093 -0.127674,-2.04136 z m 131.94034,-28.04583 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -131.62344,-22.93055 c 0,-0.87313 0.0688,-1.23032 0.152883,-0.79375 0.08408,0.43656 0.08408,1.15093 0,1.5875 -0.08409,0.43656 -0.152883,0.0794 -0.152883,-0.79375 z m 344.64966,-2.11667 c 0,-0.6791 0.0726,-0.95691 0.16133,-0.61736 0.0887,0.33955 0.0887,0.89517 0,1.23472 -0.0887,0.33955 -0.16133,0.0617 -0.16133,-0.61736 z m -192.61032,-1.67564 c 11.2051,-0.0517 29.54073,-0.0517 40.74583,0 11.2051,0.0517 2.03729,0.094 -20.37292,0.094 -22.41021,0 -31.57802,-0.0423 -20.37291,-0.094 z m 192.20355,-11.58292 c 0.0169,-0.41097 0.10052,-0.49456 0.21313,-0.21314 0.10191,0.25466 0.0894,0.55893 -0.0278,0.67616 -0.11723,0.11722 -0.20061,-0.0911 -0.18529,-0.46302 z m -297.856388,-37.7909 c 0.438824,-0.0844 1.073824,-0.0812 1.411112,0.007 0.337287,0.0883 -0.02175,0.15733 -0.797864,0.15342 -0.776111,-0.004 -1.052071,-0.0762 -0.613248,-0.16054 z m 254.525058,0.006 c 0.33955,-0.0887 0.89518,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16134 -0.61736,0.16134 -0.67909,0 -0.95691,-0.0726 -0.61736,-0.16134 z M 65.49303,-222.2101 c 1.707272,-0.0648 4.406022,-0.0645 5.997223,7.3e-4 1.591204,0.0652 0.194345,0.11824 -3.104127,0.11784 -3.298473,-4.1e-4 -4.600364,-0.0538 -2.893096,-0.11857 z m 170.19796,9.8e-4 c 1.4067,-0.067 3.70858,-0.067 5.11528,0 1.4067,0.067 0.25576,0.12174 -2.55764,0.12174 -2.8134,0 -3.96434,-0.0548 -2.55764,-0.12174 z'),
									$elm$svg$Svg$Attributes$style('fill:#767676;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1095'),
									$elm$svg$Svg$Attributes$d('m 70.767376,123.16294 c 0.921632,-0.0727 2.429757,-0.0727 3.351389,0 0.921632,0.0727 0.167569,0.13213 -1.675695,0.13213 -1.843264,0 -2.597326,-0.0595 -1.675694,-0.13213 z m 142.698614,10e-4 c 0.82462,-0.0743 2.17399,-0.0743 2.99861,0 0.82462,0.0743 0.14993,0.13505 -1.4993,0.13505 -1.64924,0 -2.32393,-0.0608 -1.49931,-0.13505 z m -190.147226,-0.3281 c 0.242534,-0.0979 0.639409,-0.0979 0.881944,0 0.242535,0.0979 0.0441,0.17793 -0.440972,0.17793 -0.48507,0 -0.683507,-0.0801 -0.440972,-0.17793 z m 241.829176,-0.01 c 0.33954,-0.0887 0.89517,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16133 -0.61736,0.16133 -0.6791,0 -0.95691,-0.0726 -0.61736,-0.16133 z m 53.9425,-42.090197 c 0,-0.485069 0.0801,-0.683507 0.17794,-0.440972 0.0979,0.242535 0.0979,0.63941 0,0.881945 -0.0979,0.242534 -0.17794,0.0441 -0.17794,-0.440973 z M -25.53307,71.563912 c 0,-0.873125 0.0688,-1.230313 0.152883,-0.79375 0.08408,0.436562 0.08408,1.150937 0,1.5875 -0.08409,0.436562 -0.152883,0.07937 -0.152883,-0.79375 z M 106.08892,54.45419 c 7.1e-4,-2.134305 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z m 213.42382,-4.233333 c -0.001,-1.74625 0.0586,-2.505103 0.13257,-1.686338 0.074,0.818762 0.0749,2.247512 0.002,3.175 -0.0731,0.927485 -0.13361,0.257588 -0.13468,-1.488662 z M -25.845986,16.883359 c 0,-2.425347 0.05636,-3.417535 0.12525,-2.204861 0.06889,1.212673 0.06889,3.197048 0,4.409722 -0.06889,1.212673 -0.12525,0.220486 -0.12525,-2.204861 z M 187.92234,14.590303 c 0.001,-1.552222 0.0633,-2.14334 0.13783,-1.313592 0.0745,0.829747 0.0735,2.099747 -0.002,2.822222 -0.0758,0.722475 -0.13678,0.04359 -0.1355,-1.50863 z M 105.63203,-7.6640924 c 0.0169,-0.4109685 0.10052,-0.4945592 0.21314,-0.2131378 0.10191,0.2546597 0.0894,0.558934 -0.0278,0.6761586 -0.11722,0.1172245 -0.2006,-0.091137 -0.18528,-0.4630208 z m -3.28472,-0.7554383 c 0.63406,-0.078514 1.58656,-0.076895 2.11666,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.15284,0.142755 -1.16417,-0.00212 -1.59789,-0.067839 -0.96382,-0.1463533 z m 87.48889,0 c 0.63406,-0.078514 1.58656,-0.076895 2.11666,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.15284,0.142755 -1.16417,-0.00212 -1.59789,-0.067839 -0.96382,-0.1463533 z M 35.665986,-8.7749473 c 0.824618,-0.07427 2.173993,-0.07427 2.998611,0 0.824618,0.074274 0.149931,0.1350433 -1.499305,0.1350433 -1.649237,0 -2.323924,-0.06077 -1.499306,-0.1350433 z m 219.614814,-0.00141 c 0.92749,-0.072912 2.35624,-0.07203 3.175,0.00212 0.81877,0.073984 0.0599,0.1336392 -1.68634,0.1325633 -1.74625,-0.00106 -2.41614,-0.061609 -1.48866,-0.1345177 z m 64.25552,-72.7652777 c 0,-3.977569 0.0514,-5.604757 0.11433,-3.615972 0.0629,1.988784 0.0629,5.243159 0,7.231944 -0.0629,1.988785 -0.11433,0.361597 -0.11433,-3.615972 z M 86.113209,-90.61628 c 0.630591,-0.07825 1.662466,-0.07825 2.293056,0 0.63059,0.07826 0.114653,0.142286 -1.146528,0.142286 -1.26118,0 -1.777118,-0.06403 -1.146528,-0.142286 z m 119.597991,-3.52e-4 c 0.63406,-0.07852 1.58656,-0.0769 2.11666,0.0036 0.53011,0.0805 0.0113,0.144734 -1.15284,0.142755 -1.16417,-0.0021 -1.59789,-0.06784 -0.96382,-0.146353 z m -100.07917,-0.655673 c 0.0169,-0.410969 0.10052,-0.494559 0.21314,-0.213138 0.10191,0.25466 0.0894,0.558934 -0.0278,0.676159 -0.11722,0.117224 -0.2006,-0.09114 -0.18528,-0.463021 z m -131.482,-19.020715 c 7.96e-4,-2.13431 0.0589,-2.96195 0.129121,-1.83921 0.07022,1.12275 0.06957,2.869 -0.0014,3.88056 -0.07102,1.01156 -0.128469,0.093 -0.127674,-2.04135 z m 213.76998,-3.35139 c 0,-1.45521 0.0623,-2.05052 0.13838,-1.32292 0.0761,0.72761 0.0761,1.91823 0,2.64583 -0.0761,0.72761 -0.13838,0.1323 -0.13838,-1.32291 z m -81.82964,-39.86389 c 0,-2.23132 0.0573,-3.14413 0.12729,-2.02847 0.07,1.11566 0.07,2.94128 0,4.05694 -0.07,1.11566 -0.12729,0.20285 -0.12729,-2.02847 z m -131.63056,-12.52361 c 0.0039,-0.77611 0.07616,-1.05207 0.160542,-0.61325 0.08438,0.43883 0.08118,1.07383 -0.0071,1.41111 -0.0883,0.33729 -0.157337,-0.0217 -0.153422,-0.79786 z m 344.68065,-1.94028 c 0,-1.06715 0.0661,-1.50371 0.14696,-0.97014 0.0808,0.53358 0.0808,1.40671 0,1.94028 -0.0808,0.53358 -0.14696,0.097 -0.14696,-0.97014 z m -12.14516,-40.83402 c -0.43881,-0.55953 -0.42633,-0.57201 0.13319,-0.1332 0.33955,0.2663 0.61736,0.54411 0.61736,0.61737 0,0.29034 -0.28969,0.10347 -0.75055,-0.48417 z M 23.142375,-221.83759 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339549,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679097,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 250.478945,-0.001 c 0.34324,-0.0894 0.81949,-0.0836 1.05833,0.0131 0.23884,0.0966 -0.042,0.16982 -0.62408,0.16263 -0.58208,-0.007 -0.7775,-0.0863 -0.43425,-0.1757 z m -197.904412,-0.36804 c 0.927488,-0.0729 2.356238,-0.072 3.175,0.002 0.818766,0.074 0.05991,0.13364 -1.686338,0.13256 -1.74625,-0.001 -2.416147,-0.0616 -1.488662,-0.13452 z m 152.049462,-0.001 c 1.12274,-0.0702 2.86899,-0.0696 3.88056,0.001 1.01156,0.071 0.093,0.12847 -2.04136,0.12767 -2.1343,-8e-4 -2.96194,-0.0589 -1.8392,-0.12912 z'),
									$elm$svg$Svg$Attributes$style('fill:#606060;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path1093'),
									$elm$svg$Svg$Attributes$d('m 77.117376,123.16294 c 0.921632,-0.0727 2.429757,-0.0727 3.351389,0 0.921632,0.0727 0.167569,0.13213 -1.675695,0.13213 -1.843264,0 -2.597326,-0.0595 -1.675694,-0.13213 z m 130.704174,10e-4 c 0.82461,-0.0743 2.17399,-0.0743 2.99861,0 0.82461,0.0743 0.14993,0.13505 -1.49931,0.13505 -1.64924,0 -2.32392,-0.0608 -1.4993,-0.13505 z m -182.915286,-0.3376 c 0.339548,-0.0887 0.895173,-0.0887 1.234722,0 0.339548,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679098,0 -0.95691,-0.0726 -0.617361,-0.16134 z m 238.125006,0 c 0.33955,-0.0887 0.89517,-0.0887 1.23472,0 0.33955,0.0887 0.0617,0.16134 -0.61736,0.16134 -0.6791,0 -0.95691,-0.0726 -0.61736,-0.16134 z m -275.979415,-12.72146 -1.215731,-1.32291 1.322917,1.21573 c 0.727604,0.66865 1.322916,1.26396 1.322916,1.32291 0,0.26939 -0.298526,0.0156 -1.430102,-1.21573 z M 319.06259,79.472014 c 0.0169,-0.410968 0.10052,-0.494559 0.21314,-0.213138 0.10191,0.25466 0.0894,0.558934 -0.0278,0.676159 -0.11722,0.117225 -0.2006,-0.09114 -0.18528,-0.463021 z M -25.53307,68.741689 c 0,-0.873124 0.0688,-1.230312 0.152883,-0.793749 0.08408,0.436562 0.08408,1.150937 0,1.587499 -0.08409,0.436563 -0.152883,0.07937 -0.152883,-0.79375 z m 131.62199,-6.879166 c 7.1e-4,-2.134305 0.0589,-2.961947 0.12912,-1.839203 0.0702,1.122743 0.0696,2.868993 -0.001,3.880555 -0.071,1.011562 -0.12847,0.09295 -0.12767,-2.041352 z M 319.53457,40.695858 c 3.4e-4,-3.686528 0.0526,-5.146721 0.11617,-3.244871 0.0636,1.901849 0.0633,4.918099 -7.1e-4,6.702777 -0.0639,1.784678 -0.11589,0.228621 -0.11555,-3.457906 z M 187.91717,19.529192 c 0.001,-1.358194 0.0653,-1.87059 0.14169,-1.138661 0.0764,0.731933 0.0751,1.843183 -0.003,2.469445 -0.0779,0.626265 -0.14041,0.02741 -0.13885,-1.330784 z M -25.848522,8.7694703 c 0,-2.2313193 0.05728,-3.1441317 0.127289,-2.0284721 0.07001,1.1156597 0.07001,2.9412846 0,4.0569438 -0.07001,1.11566 -0.127289,0.202848 -0.127289,-2.0284717 z M 105.72279,-4.6360845 c 0,-1.4552083 0.0623,-2.0505207 0.13838,-1.3229166 0.0761,0.7276041 0.0761,1.918229 0,2.6458332 -0.0761,0.7276041 -0.13838,0.1322916 -0.13838,-1.3229166 z m -7.25604,-3.7834462 c 0.634065,-0.078514 1.58657,-0.076895 2.11667,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.152849,0.142755 -1.164167,-0.00212 -1.597886,-0.067839 -0.963821,-0.1463533 z m 95.25,0 c 0.63407,-0.078514 1.58657,-0.076895 2.11667,0.0036 0.5301,0.080497 0.0113,0.1447341 -1.15285,0.142755 -1.16416,-0.00212 -1.59788,-0.067839 -0.96382,-0.1463533 z M 29.855796,-8.7763796 c 0.927488,-0.072912 2.356238,-0.07203 3.175,0.00212 0.818765,0.073985 0.05991,0.1336393 -1.686338,0.1325633 -1.74625,-0.00106 -2.416147,-0.061609 -1.488662,-0.1345177 z m 231.235194,0.00141 c 0.82462,-0.07427 2.17399,-0.07427 2.99861,0 0.82462,0.074274 0.14993,0.1350433 -1.4993,0.1350433 -1.64924,0 -2.32393,-0.06077 -1.49931,-0.1350433 z m 58.44358,-59.1847234 c 3.4e-4,-3.686528 0.0526,-5.146721 0.11617,-3.244871 0.0636,1.901849 0.0633,4.918099 -7.1e-4,6.702777 -0.0639,1.784678 -0.11589,0.228621 -0.11555,-3.457906 z M 82.238972,-90.616749 c 0.634065,-0.07851 1.586565,-0.0769 2.116667,0.0036 0.530101,0.0805 0.01132,0.144734 -1.152846,0.142755 -1.164167,-0.0021 -1.597886,-0.06784 -0.963821,-0.146355 z m 127.352778,0 c 0.63407,-0.07851 1.58657,-0.0769 2.11667,0.0036 0.5301,0.0805 0.0113,0.144734 -1.15285,0.142755 -1.16416,-0.0021 -1.59788,-0.06784 -0.96382,-0.146355 z m -103.86896,-3.624886 c 0,-1.455209 0.0623,-2.050521 0.13838,-1.322917 0.0761,0.727604 0.0761,1.918229 0,2.645833 -0.0761,0.727604 -0.13838,0.132292 -0.13838,-1.322916 z m -131.574207,-8.819445 c 0,-2.03729 0.05831,-2.87073 0.129568,-1.85208 0.07126,1.01864 0.07126,2.68552 0,3.70416 -0.07126,1.01865 -0.129568,0.18521 -0.129568,-1.85208 z M 187.92001,-118.5833 c 0,-1.45521 0.0623,-2.05052 0.13838,-1.32292 0.0761,0.72761 0.0761,1.91823 0,2.64584 -0.0761,0.7276 -0.13838,0.13229 -0.13838,-1.32292 z m -81.83254,-42.33333 c 0,-2.03729 0.0583,-2.87073 0.12957,-1.85209 0.0713,1.01865 0.0713,2.68553 0,3.70417 -0.0713,1.01865 -0.12957,0.18521 -0.12957,-1.85208 z m -131.62766,-2.64584 c 0.0039,-0.77611 0.07616,-1.05207 0.160542,-0.61324 0.08438,0.43882 0.08118,1.07382 -0.0071,1.41111 -0.0883,0.33729 -0.157337,-0.0217 -0.153422,-0.79787 z m 344.67101,-1.23472 c 0,-0.87312 0.0688,-1.23031 0.15289,-0.79375 0.0841,0.43657 0.0841,1.15094 0,1.5875 -0.0841,0.43657 -0.15289,0.0794 -0.15289,-0.79375 z m -293.871779,-57.0404 c 0.339549,-0.0887 0.895174,-0.0887 1.234723,0 0.339548,0.0887 0.06174,0.16134 -0.617361,0.16134 -0.679098,0 -0.95691,-0.0726 -0.617362,-0.16134 z m 246.768059,0.01 c 0.24254,-0.0979 0.63941,-0.0979 0.88195,0 0.24253,0.0979 0.0441,0.17793 -0.44098,0.17793 -0.48506,0 -0.6835,-0.0801 -0.44097,-0.17793 z m -190.323613,-0.37845 c 0.921632,-0.0727 2.429757,-0.0727 3.351389,0 0.921632,0.0727 0.16757,0.13213 -1.675694,0.13213 -1.843264,0 -2.597327,-0.0595 -1.675695,-0.13213 z m 138.654553,-0.002 c 1.12274,-0.0702 2.86899,-0.0696 3.88055,0.001 1.01156,0.071 0.093,0.12847 -2.04135,0.12767 -2.13431,-8e-4 -2.96195,-0.0589 -1.8392,-0.12912 z'),
									$elm$svg$Svg$Attributes$style('fill:#494949;fill-opacity:1;stroke-width:0.352778')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$g,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$transform('matrix(0.26458333,0,0,0.26458333,21.364114,-256.04852)'),
									$elm$svg$Svg$Attributes$id('g1187')
								]),
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$g,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$transform('translate(-283.45177,55.633036)'),
											$elm$svg$Svg$Attributes$id('g1257')
										]),
									_List_fromArray(
										[
											A2(
											$elm$svg$Svg$g,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$id('g1261')
												]),
											_List_fromArray(
												[
													A2(
													$elm$svg$Svg$circle,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$id('circle1243'),
															$elm$svg$Svg$Attributes$fill('#aae0fa'),
															$elm$svg$Svg$Attributes$r('300'),
															$elm$svg$Svg$Attributes$cy('584.64453'),
															$elm$svg$Svg$Attributes$cx('1530.6154')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$id('path1245'),
															$elm$svg$Svg$Attributes$fill('#061922'),
															$elm$svg$Svg$Attributes$d('m 1635.5353,786.96456 c -28.722,29.23 -64.1,43.842 -106.13,43.842 -47.17,0 -84.59,-16.14 -112.27,-48.44 -26.15,-30.762 -39.22,-69.972 -39.22,-117.64 0,-51.26 22.302,-109.72 66.9,-175.34 36.38,-53.814 79.19,-100.98 128.41,-141.48 -7.182,32.814 -10.758,56.13 -10.758,69.972 0,31.794 9.984,62.802 29.976,93.05 24.612,35.88 43.31,62.56 56.14,79.968 19.992,30.26 29.988,59.73 29.988,88.42 10e-4,42.558 -14.346,78.44 -43.04,107.65 m -0.774,-164.17 c -7.686,-17.17 -16.662,-28.572 -26.916,-34.22 1.536,3.084 2.31,7.44 2.31,13.08 0,10.77 -3.072,26.14 -9.234,46.13 l -9.984,30.762 c 0,17.94 8.952,26.916 26.904,26.916 18.96,0 28.452,-12.57 28.452,-37.686 0,-12.804 -3.84,-27.792 -11.532,-44.988')
														]),
													_List_Nil)
												]))
										])),
									A2(
									$elm$svg$Svg$g,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$id('g1333'),
											$elm$svg$Svg$Attributes$transform('translate(-392.14693,334.27757)')
										]),
									_List_fromArray(
										[
											A2(
											$elm$svg$Svg$circle,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$cx('300'),
													$elm$svg$Svg$Attributes$cy('300'),
													$elm$svg$Svg$Attributes$r('300'),
													$elm$svg$Svg$Attributes$fill('#9bd3ae'),
													$elm$svg$Svg$Attributes$id('circle1315')
												]),
											_List_Nil),
											A2(
											$elm$svg$Svg$path,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$d('m 562.6,337.4 c 0,10 -3.9,19 -11.6,27 -7.7,8 -16.6,12 -26.6,12 -16,0 -27.7,-7.5 -35.2,-22.5 L 454,352.4 c -7.5,0 -22.3,3.3 -44.2,9.8 -23.5,6.5 -37,11.7 -40.5,15.7 -5.5,6 -10,20 -13.5,42 -3,18 -4.5,31.2 -4.5,39.7 0,13.5 2.1,23.4 6.4,29.6 4.3,6.2 13,11.5 26.2,15.7 13.2,4.2 21.4,6.6 24.4,7.1 2,0 5.2,-0.2 9.8,-0.7 h 9 c 6.5,0 13.2,1 20.2,3 10,3 14.3,7 12.8,12 -7,-1 -19.2,0.5 -36.7,4.5 l 21,10.5 c 0,6 -8.5,9 -25.5,9 -4.5,0 -10.6,-1 -18.4,-3 -7.7,-2 -12.9,-3 -15.4,-3 h -9.7 c -0.5,5 -2,12.5 -4.5,22.5 -8.5,-0.5 -18.5,-5.5 -30,-15 -11.5,-9.5 -18.7,-14.2 -21.7,-14.2 -3,0 -7.3,4.8 -12.7,14.2 -5.5,9.5 -8.2,16 -8.2,19.5 -6.5,-3.5 -12,-10 -16.5,-19.5 -2,-6.5 -4.2,-13 -6.7,-19.5 -5,0.5 -14.2,11 -27.7,31.5 h -3.8 c -1,-1.5 -4.8,-12 -11.2,-31.5 -15.5,-5 -30,-7.5 -43.5,-7.5 -6.5,0 -16.5,1.5 -30,4.5 l -21,-1.5 c 3,-3 11.7,-8.7 26.2,-17.2 17,-10 30,-15 39,-15 1.5,0 3.5,0.3 6,0.8 2.5,0.5 4.5,0.8 6,0.8 3.5,0 9.1,-1.9 16.9,-5.6 7.7,-3.7 12.2,-7.1 13.5,-10.1 1.3,-3 1.9,-10.8 1.9,-23.2 0,-28.5 -7.5,-49.7 -22.5,-63.7 -13,-12.5 -34.5,-21.5 -64.5,-27 -8,28.5 -30.5,42.7 -67.4,42.7 -12,0 -24,-7.2 -36,-21.7 -12.3,-14.8 -18.3,-28 -18.3,-40 0,-18.5 7.7,-33.7 23.2,-45.7 -12.5,-13 -18.7,-26.2 -18.7,-39.7 0,-12.5 3.9,-23.5 11.6,-33 7.7,-9.5 17.9,-15 30.4,-16.5 -1,-16 4.2,-27 15.7,-33 -5.5,-5.5 -8.2,-15.2 -8.2,-29.2 0,-16.5 5.5,-30.2 16.5,-41.2 11,-11 24.7,-16.5 41.2,-16.5 18,0 32.7,6.3 44.2,18.8 14.5,-49.5 45.7,-74.2 93.7,-74.2 25,0 47,10 66,30 7,7.5 10.5,11.5 10.5,12 -6,0 -3,-1.1 9,-3.4 12,-2.2 20.7,-3.4 26.2,-3.4 19.5,0 36.7,7.2 51.7,21.7 13,13 22,29.5 27,49.5 3.5,0.5 9,2 16.5,4.5 11,5.5 16.5,15 16.5,28.5 0,2.5 -2,7.3 -6,14.2 32,18 48,43 48,75 0,9 -3.5,21.5 -10.5,37.5 13,7.5 19.5,18.5 19.5,33 m -308.8,33 v -9.7 c 0,-11.5 -5.6,-22 -16.9,-31.5 -11.2,-9.5 -22.6,-14.2 -34.1,-14.2 -14,0 -27,3.2 -39,9.7 26.5,-1.5 56.5,13.8 89.9,45.7 m -13.5,-92.9 c -7.5,-8.5 -14,-17.2 -19.5,-26.2 -21,5.5 -31.5,11.7 -31.5,18.7 6,-0.5 14.7,0.6 26.2,3.4 11.5,2.8 19.7,4.1 24.8,4.1 M 286,254.4 v -33 c -12,-2 -19.3,-3 -21.7,-3 v 11.2 l 21.7,24.7 m 97.4,-21 c -6,-2.5 -17.2,-7.5 -33.7,-15 v 64.5 c 23.5,-13.5 34.7,-30 33.7,-49.5 m 41.2,88.5 -16.5,-20.2 c -10,7 -20.1,14.1 -30.4,21.4 -10.3,7.2 -19.1,15.4 -26.6,24.4 22.5,-12 47,-20.5 73.4,-25.5'),
													$elm$svg$Svg$Attributes$fill('#00160b'),
													$elm$svg$Svg$Attributes$id('path1317')
												]),
											_List_Nil)
										])),
									A2(
									$elm$svg$Svg$g,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$id('g1397'),
											$elm$svg$Svg$Attributes$transform('translate(683.75433,1125.8358)')
										]),
									_List_fromArray(
										[
											A2(
											$elm$svg$Svg$g,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$id('g1480')
												]),
											_List_fromArray(
												[
													A2(
													$elm$svg$Svg$circle,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$cx('300'),
															$elm$svg$Svg$Attributes$cy('300'),
															$elm$svg$Svg$Attributes$r('300'),
															$elm$svg$Svg$Attributes$fill('#cbc2bf'),
															$elm$svg$Svg$Attributes$id('circle1383')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$d('m 544.2,291.7 c 0,33.1 -12,55.7 -36.1,67.7 -7,3.5 -29.1,8.3 -66.2,14.3 -24.1,4 -36.1,13.3 -36.1,27.8 v 60.9 c 0,2.5 0.8,10.3 2.3,23.3 l 2.3,24.1 c 0,7.5 -1.8,19.8 -5.3,36.9 -9.5,2 -20.6,4.3 -33.1,6.8 -4,-15.1 -6,-25.3 -6,-30.9 0,-2.5 0.6,-6.3 1.9,-11.3 1.2,-5 1.9,-8.8 1.9,-11.3 0,-3.5 -3.1,-13.3 -9.4,-29.3 h -11.7 c -1.5,2.5 -2.1,5.8 -1.6,9.8 2,8.5 2.8,15.8 2.3,21.8 -8.5,6 -20.3,14.1 -35.4,24.1 -3.5,-1 -4.8,-1.5 -3.8,-1.5 v -53.4 c -1,-2.5 -3.5,-3.5 -7.5,-3 h -9 l -9,70.7 c -7,0.5 -15.6,0.5 -25.6,0 -3.5,-16.5 -9.8,-41.1 -18.8,-73.7 h -6 c -5.5,17.6 -8.5,27.1 -9,28.6 0,2 0.6,5.9 1.9,11.7 1.2,5.8 1.9,9.7 1.9,11.7 0,1.5 -0.5,5.3 -1.5,11.3 l -2.3,18.1 c -1,1 -2.3,1.5 -3.8,1.5 -15,0 -25.1,-3.8 -30.1,-11.3 -5,-7.5 -7,-18.1 -6,-31.6 l 6,-90.3 c 0,-1.5 0.5,-3.5 1.5,-6 1,-2.5 1.5,-4.3 1.5,-5.3 0,-4 -4.3,-12 -12.8,-24.1 -1.5,-0.5 -9.3,-2.3 -23.3,-5.3 C 149.8,372.5 133,369 107.9,364 73.3,357.5 56,329.7 56,280.5 56,207.3 86.1,146.3 146.3,97.7 c 2.5,13.5 6.8,31.6 12.8,54.2 4.5,1 14.3,3.3 29.3,6.8 3,1 18.3,6.5 45.9,16.6 -14.1,-8.5 -32.4,-22.3 -54.9,-41.4 -8.5,-10 -12.8,-26.8 -12.8,-50.4 0,-5.5 9.5,-12 28.6,-19.6 17,-7 29.9,-11 38.4,-12 27.1,-3.5 47.9,-5.3 62.5,-5.3 62.7,0 113.4,16.1 152,48.2 -12.5,14.6 -34.1,30.1 -64.7,46.6 12.1,0.5 29.6,-4.2 52.7,-14.3 23.1,-10 32.9,-15 29.3,-15 4,0 12.1,8 24.1,24.1 9,12 16.3,22.8 21.8,32.4 16,28.6 26.8,59.5 32.4,92.6 0,11.6 0.2,19.8 0.8,24.8 v 6 0 z M 256,305.2 c 0,-21.6 -9.4,-42 -28.2,-61.3 -18.8,-19.3 -39,-29 -60.6,-29 -19.1,0 -35.9,8.1 -50.4,24.2 -14.6,16.2 -21.8,34.1 -21.8,53.8 0,17.2 8.3,28.3 24.8,33.3 10.5,3 25.3,4.8 44.4,5.3 h 41.4 c 33.6,0.5 50.4,-8.3 50.4,-26.3 m 82,93.3 v -23.3 c -3.5,-6.5 -7,-13.3 -10.5,-20.3 -3,-10 -8.5,-24.1 -16.6,-42.1 l -8.3,88 c 0,7 -1.5,10.5 -4.5,10.5 -2,0 -3.5,-0.5 -4.5,-1.5 -3.5,-53.2 -5.3,-76.2 -5.3,-69.2 v -26.3 c -1,-1.5 -2.2,-2.3 -3.7,-2.3 -17.1,17.6 -25.6,45.9 -25.6,85 0,21.6 2,34.9 6,39.9 4,-1 8.5,-2.8 13.5,-5.3 2,-1 7.8,-1.5 17.3,-1.5 9.5,0 21.1,3 34.6,9 5,0 7.5,-13.5 7.5,-40.6 M 508,293.7 c 0,-20.2 -7.5,-38.2 -22.6,-54.1 -15.1,-15.9 -32.4,-23.8 -51.9,-23.8 -21.1,0 -40.8,9.6 -59.1,29 -18.3,19.3 -27.5,39.5 -27.5,60.6 0,17.6 8.5,26.3 25.6,26.3 H 459 c 32.6,-0.5 48.9,-13.1 48.9,-37.9'),
															$elm$svg$Svg$Attributes$fill('#130c0e'),
															$elm$svg$Svg$Attributes$id('path1385')
														]),
													_List_Nil)
												]))
										])),
									A2(
									$elm$svg$Svg$g,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$id('g1466'),
											$elm$svg$Svg$Attributes$transform('translate(267.95451,667.52066)')
										]),
									_List_fromArray(
										[
											A2(
											$elm$svg$Svg$g,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$transform('translate(-445.20019,458.31514)'),
													$elm$svg$Svg$Attributes$id('g1461')
												]),
											_List_fromArray(
												[
													A2(
													$elm$svg$Svg$g,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$id('g1484')
														]),
													_List_fromArray(
														[
															A2(
															$elm$svg$Svg$circle,
															_List_fromArray(
																[
																	$elm$svg$Svg$Attributes$id('circle1447'),
																	$elm$svg$Svg$Attributes$fill('#f9aa8f'),
																	$elm$svg$Svg$Attributes$r('300'),
																	$elm$svg$Svg$Attributes$cy('300'),
																	$elm$svg$Svg$Attributes$cx('300')
																]),
															_List_Nil),
															A2(
															$elm$svg$Svg$path,
															_List_fromArray(
																[
																	$elm$svg$Svg$Attributes$id('path1449'),
																	$elm$svg$Svg$Attributes$fill('#200000'),
																	$elm$svg$Svg$Attributes$d('m 551.8,399.7 c -22.4,53.5 -67,80.2 -133.6,80.2 -12.2,0 -25.5,1.5 -39.7,4.6 -21.4,4.6 -32.1,11 -32.1,19.1 0,2.5 1.8,5.5 5.3,8.8 3.6,3.3 6.6,5 9.2,5 -12.7,0 -4.1,0.4 26,1.1 30.1,0.8 48.9,1.1 56.5,1.1 -44.3,26 -118.4,37.9 -222.3,35.9 C 187,555 157.7,540 133.3,510.4 c -24,-28 -35.9,-59.3 -35.9,-93.9 0,-36.6 12.3,-67.8 37.1,-93.6 24.7,-25.7 55.4,-38.6 92,-38.6 8.1,0 19,1.8 32.5,5.3 13.5,3.6 22.5,5.3 27.1,5.3 18.8,0 42.3,-7.8 70.3,-23.3 28,-15.5 41.3,-23.3 39.7,-23.3 -5.1,53.5 -22.9,89.4 -53.5,107.7 -21.9,12.7 -32.8,25.2 -32.8,37.4 0,7.6 4.6,13.8 13.7,18.3 7.1,3.6 15,5.4 23.7,5.4 13.2,0 26.2,-8.1 39,-24.4 12.7,-16.3 18.3,-31.1 16.8,-44.3 -1.5,-15.3 -0.5,-33.6 3.1,-55 1,-6.1 4.7,-13.6 11.1,-22.5 6.4,-8.9 12.1,-14.4 17.2,-16.4 0,4.6 -1.6,12.2 -5,22.9 -3.3,10.7 -5,18.6 -5,23.7 0,11.2 3,19.9 9.2,26 9.2,-3.6 17.3,-15 24.4,-34.4 6.1,-14.8 9.7,-29 10.7,-42.8 -21.4,-1 -41.9,-10.7 -61.5,-29 -19.6,-18.3 -29.4,-38.2 -29.4,-59.6 0,-3.6 0.5,-7.1 1.5,-10.7 3,4.6 7.6,11.7 13.7,21.4 8.7,12.7 15.3,19.1 19.9,19.1 6.1,0 9.2,-6.4 9.2,-19.1 0,-16.3 -4.3,-31.1 -13,-44.3 -9.7,-15.8 -22.2,-23.7 -37.4,-23.7 -7.1,0 -17.8,3.8 -32.1,11.5 -14.3,7.6 -27.3,11.5 -39,11.5 -3.6,0 -19.4,-4.6 -47.4,-13.8 49.4,-8.1 74.1,-15.5 74.1,-22.1 0,-17.3 -33.9,-29 -101.6,-35.1 -6.6,-0.5 -18.8,-1.5 -36.7,-3.1 2,-2.5 16.5,-5.3 43.5,-8.4 22.9,-2.5 39,-3.8 48.1,-3.8 121.2,0 198.1,58.8 230.7,176.5 5.6,-4.6 8.4,-12.4 8.4,-23.2 0,-13.9 -4.1,-31.5 -12.2,-52.7 -3.1,-8.2 -7.9,-20.6 -14.5,-37.2 41.7,53.2 62.6,103.6 62.6,151.2 0,25.1 -5.9,47.8 -17.6,68.3 -7.6,13.8 -21.9,31.5 -42.8,53 -20.9,21.5 -35.1,38.1 -42.8,49.9 28,-7.6 46.4,-13.5 55,-17.6 19.3,-8.6 36.9,-21.6 52.7,-39 0,6.6 -2.8,16.6 -8.4,29.8 M 218.8,99.5 c 0,9.2 -5.1,15 -15.3,17.6 l -19.9,3.1 c -7.1,3.6 -17.6,17.6 -31.3,42 -1.5,-7.6 -3.8,-18.3 -6.9,-32.1 -4.6,0.5 -12.2,4.6 -22.9,12.2 -4.6,3.6 -12,8.9 -22.2,16 3.1,-18.3 13.2,-36.9 30.6,-55.8 18.3,-20.9 36.2,-31.3 53.5,-31.3 22.9,0 34.4,9.4 34.4,28.3 m 132.9,70.3 c 0,8.7 -4.7,15.9 -14.1,21.8 -9.4,5.9 -18.7,8.8 -27.9,8.8 -12.2,0 -23.2,-6.9 -32.8,-20.6 -11.7,-16.8 -23.7,-27.7 -35.9,-32.9 2.5,-2.5 5.6,-3.8 9.2,-3.8 4.6,0 12.3,3.6 23.3,10.7 10.9,7.1 17.9,10.7 21,10.7 2.5,0 6.7,-3.6 12.6,-10.7 5.9,-7.1 12.3,-10.7 19.5,-10.7 16.8,0 25.2,8.9 25.2,26.7')
																]),
															_List_Nil)
														]))
												]))
										])),
									A2(
									$elm$svg$Svg$g,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$id('g1265'),
											$elm$svg$Svg$Attributes$transform('translate(28.9,-289.2)')
										]),
									_List_fromArray(
										[
											A2(
											$elm$svg$Svg$circle,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$cx('536.13031'),
													$elm$svg$Svg$Attributes$cy('489.68692'),
													$elm$svg$Svg$Attributes$r('300'),
													$elm$svg$Svg$Attributes$fill('#fffbd5'),
													$elm$svg$Svg$Attributes$id('circle1173')
												]),
											_List_Nil),
											A2(
											$elm$svg$Svg$path,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$d('m 822.33033,532.08692 c -39.4,-22.2 -64.6,-33.3 -75.7,-33.3 -8.1,0 -14.4,6.2 -18.9,18.6 -4.5,12.4 -13.6,18.5 -27.2,18.5 -5.6,0 -16.9,-2 -34.1,-6 -9.6,14.6 -14.4,24 -14.4,28 0,5.6 4.1,12.1 12.4,19.7 8.3,7.6 15.2,11.3 20.9,11.3 3.6,0 8.5,-0.7 14.7,-2.3 6.2,-1.5 10.3,-2.3 12.4,-2.3 6.2,0 9.3,11.4 9.3,34.1 0,21.7 -5,55 -15.1,99.9 -13.1,-51.5 -27,-77.2 -41.6,-77.2 -2,0 -6.2,1.5 -12.5,4.6 -6.3,3 -11,4.5 -14,4.5 -14.6,0 -27.7,-13.4 -39.4,-40.1 -23.2,3.5 -34.8,15.4 -34.8,35.6 0,10.1 4.7,18.2 14,24.2 9.3,6.1 14,10.4 14,12.9 0,13.6 -19.9,34.6 -59.8,62.8 -21.2,15.1 -35.8,25.7 -43.9,31.8 7,-9.1 14.1,-20.9 21.2,-35.6 8.1,-16.6 12.1,-29.5 12.1,-38.6 0,-5 -5.8,-12.1 -17.4,-21.2 -11.6,-9.1 -17.4,-18.7 -17.4,-28.8 0,-8.6 3,-19.2 9.1,-31.8 -6.6,-7.6 -14.4,-11.4 -23.5,-11.4 -20.2,0 -30.3,6.6 -30.3,19.7 0,-9.1 0,-2.3 0,20.4 0.5,16.7 -12.1,25 -37.9,25 -19.7,0 -52.7,-4.6 -99.2,-13.6 52.5,-13.1 78.7,-28.3 78.7,-45.4 0,2 -1,-4 -3,-18.2 -2,-15.6 9.1,-29.8 33.3,-42.4 -4.5,-23.2 -16.6,-34.8 -36.3,-34.8 -3,0 -8.6,5.3 -16.6,15.9 -8.1,10.6 -15.6,15.9 -22.7,15.9 -12.1,0 -27.8,-13.1 -46.9,-39.4 -9.1,-13.1 -23,-32.5 -41.6,-58.3 11.6,6.1 23.2,12.1 34.8,18.2 15.1,7.1 27.3,10.6 36.3,10.6 7.1,0 14,-6.2 20.8,-18.6 6.8,-12.4 15.8,-18.6 26.9,-18.6 1.5,0 11.6,3 30.3,9.1 9.6,-14.6 14.4,-25.5 14.4,-32.6 0,-6.1 -3.7,-13 -11,-20.8 -7.3,-7.8 -14,-11.7 -20.1,-11.7 -2.5,0 -6.4,0.8 -11.7,2.3 -5.3,1.5 -9.2,2.3 -11.7,2.3 -9.1,0 -13.6,-11.4 -13.6,-34.1 0,-6.1 5.8,-40.6 17.4,-103.7 -0.5,7.6 2.8,21.7 9.8,42.4 8.6,25.2 18.7,37.9 30.3,37.9 2,0 6.1,-1.5 12.1,-4.5 6.1,-3 10.8,-4.5 14.4,-4.5 11.6,0 21.2,6.6 28.8,19.7 l 11.4,20.4 c 10.6,0 19.4,-3.8 26.5,-11.3 7.1,-7.6 10.6,-16.7 10.6,-27.3 0,-11.1 -4.7,-19.6 -14,-25.4 -9.4,-5.8 -14,-10.2 -14,-13.2 0,-10.6 16.7,-28.5 50,-53.7 26.7,-20.2 44.2,-32 52.2,-35.6 -21.7,29.3 -32.6,50.7 -32.6,64.3 0,7.1 4.3,14.6 12.9,22.7 10.6,9.6 16.7,16.4 18.2,20.4 5,11.6 4.5,27.5 -1.5,47.7 13.6,9.6 24,14.4 31,14.4 14.6,0 21.9,-7.6 21.9,-22.7 0,-1.5 -0.6,-6.3 -1.9,-14.4 -1.3,-8.1 -1.6,-12.6 -1.1,-13.6 2,-7.1 15.9,-10.6 41.6,-10.6 16.2,0 49.7,4.5 100.7,13.6 -11.1,3 -27.8,7.6 -50,13.6 -20.2,6.1 -30.3,12.9 -30.3,20.4 0,3.5 1.3,9.6 3.8,18.2 2.5,8.6 3.8,14.9 3.8,18.9 0,7.1 -4.5,13.6 -13.6,19.7 l -25.7,18.2 c 6.1,11.1 10.1,17.7 12.1,19.7 5,6.1 11.9,9.1 20.4,9.1 6.1,0 11.6,-5.3 16.7,-15.9 5,-10.6 13.1,-15.9 24.2,-15.9 13.6,0 29,12.6 46.2,37.9 9.6,14.2 24.5,35.6 44.6,64.4 m -168,-43.9 c 0,-32.3 -11.9,-60.3 -35.6,-84 -23.7,-23.7 -51.7,-35.6 -84,-35.6 -32.8,0 -61.1,11.7 -84.8,35.2 -23.7,23.5 -35.8,51.6 -36.3,84.4 -0.5,32.3 11.5,60.2 36,83.6 24.5,23.5 52.9,35.2 85.2,35.2 34.3,0 63,-11.2 85.9,-33.7 23,-22.4 34.2,-50.8 33.7,-85.1 m -11.4,0 c 0,30.8 -10.3,56.3 -31,76.4 -20.7,20.2 -46.4,30.3 -77.2,30.3 -29.8,0 -55.3,-10.3 -76.4,-31 -21.2,-20.7 -31.8,-45.9 -31.8,-75.7 0,-29.3 10.7,-54.4 32.2,-75.3 21.5,-20.9 46.8,-31.4 76.1,-31.4 29.3,0 54.6,10.6 76.1,31.8 21.4,21.2 32.2,46.2 32.2,74.9'),
													$elm$svg$Svg$Attributes$fill('#211d15'),
													$elm$svg$Svg$Attributes$id('path1175')
												]),
											_List_Nil)
										]))
								]))
						]))
				]))
		]));
var $author$project$Icons$poison = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$id('svg8'),
			$elm$svg$Svg$Attributes$version('1.1'),
			$elm$svg$Svg$Attributes$viewBox('0 0 215.84996 315.42974'),
			$elm$svg$Svg$Attributes$height('64'),
			$elm$svg$Svg$Attributes$width('64')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$defs,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('defs2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$metadata,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$id('metadata5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$transform('translate(85.180792,-98.57375)'),
					$elm$svg$Svg$Attributes$id('layer1')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$transform('translate(-152.00312,122.50207)'),
							$elm$svg$Svg$Attributes$id('g969'),
							$elm$svg$Svg$Attributes$fill(
							$avh4$elm_color$Color$toCssString(
								A3($avh4$elm_color$Color$rgb, 0.2, 0.8, 0.2)))
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path985'),
									$elm$svg$Svg$Attributes$d('m 159.93613,290.03281 c 0,-0.0549 0.20836,-0.26329 0.46303,-0.46302 0.41964,-0.32909 0.429,-0.31975 0.0999,0.0999 -0.34564,0.44072 -0.56292,0.5809 -0.56292,0.36312 z m 16.93334,-14.02292 c 0,-0.0549 0.20836,-0.26329 0.46302,-0.46302 0.41964,-0.32909 0.429,-0.31975 0.0999,0.0999 -0.34564,0.44071 -0.56291,0.58089 -0.56291,0.36311 z m 15.7083,-17.89176 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 0,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 0,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m -0.0582,-9.70783 c -0.0218,-1.04643 0.0496,-1.95797 0.15879,-2.02564 0.10914,-0.0677 0.16442,0.49086 0.12284,1.24116 -0.13482,2.43293 -0.24112,2.72901 -0.28163,0.78448 z m 61.74056,-30.77342 c 0.26339,-0.29105 0.53842,-0.52917 0.61118,-0.52917 0.0728,0 -0.0832,0.23812 -0.3466,0.52917 -0.26339,0.29104 -0.53842,0.52916 -0.61118,0.52916 -0.0728,0 0.0832,-0.23812 0.3466,-0.52916 z m 3.70417,-3.70417 c 0.26339,-0.29104 0.53842,-0.52917 0.61118,-0.52917 0.0728,0 -0.0832,0.23813 -0.3466,0.52917 -0.26339,0.29104 -0.53842,0.52917 -0.61118,0.52917 -0.0728,0 0.0832,-0.23813 0.3466,-0.52917 z m -167.892368,-1.25677 -0.911798,-0.99219 0.992187,0.9118 c 0.545704,0.50149 0.992188,0.94797 0.992188,0.99219 0,0.20204 -0.223895,0.0117 -1.072577,-0.9118 z m 102.474718,-9.19427 c 0.003,-0.58209 0.0571,-0.78906 0.1204,-0.45994 0.0633,0.32912 0.0609,0.80537 -0.005,1.05834 -0.0662,0.25296 -0.11801,-0.0163 -0.11507,-0.5984 z m 0.0312,-8.86354 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m 26.36045,-4.59782 c 0,-0.0549 0.20836,-0.2633 0.46302,-0.46302 0.41964,-0.3291 0.429,-0.31975 0.0999,0.0999 -0.34564,0.44073 -0.56291,0.58089 -0.56291,0.36312 z m 2.11666,-2.11666 c 0,-0.0549 0.20836,-0.2633 0.46302,-0.46302 0.41965,-0.32911 0.42901,-0.31975 0.0999,0.0999 -0.34564,0.44073 -0.56292,0.58089 -0.56292,0.36313 z m -28.47711,-3.86886 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m -65.48372,1.91823 c -0.32911,-0.41964 -0.31975,-0.429 0.0999,-0.0999 0.25466,0.19972 0.46302,0.40808 0.46302,0.46302 0,0.21776 -0.21727,0.0776 -0.56292,-0.36313 z m 65.48372,-12.50156 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 89.35217,-7.82726 c 0.0127,-0.30822 0.0754,-0.37092 0.15985,-0.15985 0.0764,0.19099 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15045,-0.0684 -0.13896,-0.34727 z m -89.35217,-2.75607 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m -125.489496,0.90399 c 0.0127,-0.30823 0.07539,-0.37092 0.159853,-0.15985 0.07643,0.19099 0.06703,0.4192 -0.02089,0.50712 -0.08792,0.0879 -0.150453,-0.0684 -0.138965,-0.34727 z m 166.687496,-2.91042 c 0.0127,-0.30822 0.0754,-0.37092 0.15985,-0.15985 0.0764,0.19099 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15046,-0.0683 -0.13897,-0.34727 z m 48.68333,-1.05833 c 0.0127,-0.30823 0.0754,-0.37092 0.15986,-0.15985 0.0764,0.19099 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15045,-0.0684 -0.13896,-0.34727 z m -48.35765,-3.41754 c 0.002,-0.87312 0.0509,-1.19841 0.10977,-0.72286 0.0589,0.47555 0.0577,1.18992 -0.003,1.5875 -0.0604,0.39758 -0.10855,0.008 -0.10707,-0.86464 z m -118.80061,-0.66145 c 0,-0.80037 0.0496,-1.12779 0.11022,-0.72761 0.0606,0.40018 0.0606,1.05503 0,1.45521 -0.0606,0.40018 -0.11022,0.0728 -0.11022,-0.7276 z m 77.27693,-3.43959 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 89.90222,-1.05833 c 0,-0.3638 0.0601,-0.51263 0.13345,-0.33073 0.0734,0.1819 0.0734,0.47956 0,0.66146 -0.0734,0.1819 -0.13345,0.0331 -0.13345,-0.33073 z M 66.864187,128.73688 c 0,-0.50932 0.05445,-0.71768 0.121,-0.46302 0.06655,0.25466 0.06655,0.67138 0,0.92604 -0.06655,0.25467 -0.121,0.0463 -0.121,-0.46302 z m 215.330333,-2.80017 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15985 0.0764,0.19099 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15046,-0.0684 -0.13897,-0.34727 z M 67.088274,125.40754 c 0.0127,-0.30822 0.07539,-0.37092 0.159853,-0.15985 0.07643,0.191 0.06703,0.4192 -0.02089,0.50712 -0.08792,0.0879 -0.150453,-0.0684 -0.138965,-0.34727 z M 192.57777,120.5348 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 0,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.13891 -0.0972,-1.38906 z m 27.27404,-7.73907 -1.04588,-1.12447 1.12447,1.04588 c 0.61847,0.57524 1.12448,1.08125 1.12448,1.12448 0,0.20018 -0.23041,-1.3e-4 -1.20307,-1.04589 z m -27.27404,-2.844267 c 0,-1.527968 0.0437,-2.153046 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778129 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.389067 z m -0.002,-10.31875 c 0,-1.382447 0.0446,-1.947994 0.0991,-1.25677 0.0545,0.691224 0.0545,1.822317 0,2.513541 -0.0545,0.691224 -0.0991,0.125677 -0.0991,-1.256771 z M 89.953844,76.87855 c 0.263389,-0.291041 0.53842,-0.529166 0.611181,-0.529166 0.07276,0 -0.08321,0.238125 -0.346598,0.529166 -0.263389,0.291042 -0.53842,0.529167 -0.611181,0.529167 -0.07276,0 0.08321,-0.238125 0.346598,-0.529167 z M 258.32738,75.886363 c -0.3291,-0.419645 -0.31975,-0.429004 0.0999,-0.0999 0.25466,0.19972 0.46302,0.40808 0.46302,0.463021 0,0.217762 -0.21727,0.0776 -0.56292,-0.363125 z m -2.36174,-2.38125 -0.9118,-0.992188 0.99219,0.911799 c 0.92351,0.848681 1.11384,1.072576 0.9118,1.072576 -0.0442,0 -0.4907,-0.446484 -0.99219,-0.992187 z M 94.980927,71.851467 c 0.26339,-0.291042 0.538422,-0.529167 0.611182,-0.529167 0.07276,0 -0.08321,0.238125 -0.346598,0.529167 -0.263391,0.291042 -0.538422,0.529167 -0.611183,0.529167 -0.07276,0 0.08321,-0.238125 0.346599,-0.529167 z M 250.1253,68.213446 c -0.32911,-0.419645 -0.31975,-0.429003 0.0999,-0.0999 0.25466,0.199721 0.46302,0.40808 0.46302,0.463021 0,0.217763 -0.21728,0.07761 -0.56292,-0.363125 z M 192.57777,35.868134 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778125 -0.0534,0.763984 -0.0972,0.138906 -0.0972,-1.389063 z m 0,-10.583333 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763985 0.0534,2.014141 0,2.778125 -0.0534,0.763985 -0.0972,0.138907 -0.0972,-1.389062 z m 0,-10.583333 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763984 0.0534,2.014141 0,2.778125 -0.0534,0.763984 -0.0972,0.138906 -0.0972,-1.389062 z m 0,-10.5833336 c 0,-1.5279688 0.0437,-2.1530469 0.0972,-1.3890625 0.0534,0.7639843 0.0534,2.0141406 0,2.7781249 -0.0534,0.7639844 -0.0972,0.1389063 -0.0972,-1.3890624 z m 0,-10.5833333 c 0,-1.5279687 0.0437,-2.1530468 0.0972,-1.3890624 0.0534,0.7639843 0.0534,2.0141406 0,2.7781249 -0.0534,0.7639844 -0.0972,0.1389063 -0.0972,-1.3890625 z m -18.35414,-2.4811447 c 0,-0.054942 0.20836,-0.2633017 0.46303,-0.4630208 0.41964,-0.3291076 0.429,-0.3197492 0.0999,0.099895 -0.34564,0.4407292 -0.56292,0.5808872 -0.56292,0.3631261 z m 18.35414,-8.1021884 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763985 0.0534,2.014141 0,2.778125 -0.0534,0.763985 -0.0972,0.138907 -0.0972,-1.389062 z m -10.9458,1.752188 c 0,-0.05494 0.20836,-0.263301 0.46302,-0.46302 0.41964,-0.329108 0.429,-0.31975 0.0999,0.09989 -0.34564,0.44073 -0.56291,0.580888 -0.56291,0.363126 z'),
									$elm$svg$Svg$Attributes$style('fill:#000000;fill-opacity:1;stroke-width:0.264583')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path983'),
									$elm$svg$Svg$Attributes$d('m 252.01113,198.48699 c 0,-0.0549 0.20836,-0.2633 0.46302,-0.46302 0.41965,-0.32911 0.42901,-0.31975 0.0999,0.0999 -0.34564,0.44073 -0.56292,0.58089 -0.56292,0.36313 z m -157.319924,-2.3475 -0.504029,-0.59532 0.595313,0.50403 c 0.559445,0.47366 0.715761,0.6866 0.504028,0.6866 -0.05021,0 -0.318098,-0.26789 -0.595312,-0.59531 z M 255.7153,195.01501 c 0.49365,-0.50933 0.95708,-0.92604 1.02985,-0.92604 0.0728,0 -0.27161,0.41671 -0.76527,0.92604 -0.49365,0.50932 -0.95708,0.92604 -1.02984,0.92604 -0.0728,0 0.27161,-0.41672 0.76526,-0.92604 z M 91.375303,192.83219 c -0.329107,-0.41964 -0.319749,-0.429 0.0999,-0.0999 0.440729,0.34564 0.580887,0.56292 0.363126,0.56292 -0.05494,0 -0.263302,-0.20836 -0.463021,-0.46303 z M 218.27676,169.48272 c 0.26339,-0.29105 0.53842,-0.52917 0.61118,-0.52917 0.0728,0 -0.0832,0.23812 -0.3466,0.52917 -0.26339,0.29104 -0.53842,0.52916 -0.61118,0.52916 -0.0728,0 0.0832,-0.23812 0.3466,-0.52916 z m -88.27229,-1.52136 c -0.32911,-0.41964 -0.31975,-0.429 0.0999,-0.0999 0.25467,0.19972 0.46303,0.40808 0.46303,0.46302 0,0.21776 -0.21728,0.0776 -0.56292,-0.36313 z m -1.32292,-1.32291 c -0.32911,-0.41965 -0.31975,-0.42901 0.0999,-0.0999 0.25466,0.19972 0.46302,0.40808 0.46302,0.46302 0,0.21776 -0.21727,0.0776 -0.56292,-0.36312 z m 153.79844,-28.90573 c 0,-0.36381 0.0601,-0.51263 0.13345,-0.33073 0.0734,0.1819 0.0734,0.47955 0,0.66146 -0.0734,0.1819 -0.13345,0.0331 -0.13345,-0.33073 z m -167.23755,-1.47726 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15985 0.0764,0.19099 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15046,-0.0684 -0.13897,-0.34727 z m 0,-3.43958 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15986 0.0764,0.191 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15046,-0.0684 -0.13897,-0.34726 z m 167.23755,-1.69775 c 0,-0.3638 0.0601,-0.51263 0.13345,-0.33073 0.0734,0.1819 0.0734,0.47956 0,0.66146 -0.0734,0.1819 -0.13345,0.0331 -0.13345,-0.33073 z M 66.823691,129.90546 c 0.0127,-0.30823 0.07539,-0.37092 0.159852,-0.15985 0.07643,0.19099 0.06703,0.4192 -0.02089,0.50712 -0.08792,0.0879 -0.150453,-0.0684 -0.138964,-0.34727 z m 151.427929,-29.28056 -0.50403,-0.59531 0.59531,0.50403 c 0.32743,0.27721 0.59532,0.54511 0.59532,0.59531 0,0.21174 -0.21294,0.0554 -0.6866,-0.50403 z M 90.615302,76.249488 c 0,-0.05494 0.20836,-0.263301 0.463021,-0.463021 0.419644,-0.329108 0.429002,-0.319749 0.0999,0.0999 -0.345644,0.440729 -0.562916,0.580887 -0.562916,0.363125 z m 166.794648,-1.289167 -0.50403,-0.595312 0.59532,0.504028 c 0.32742,0.277215 0.59531,0.545105 0.59531,0.595313 0,0.211733 -0.21293,0.05542 -0.6866,-0.504029 z M 93.790302,73.042092 c 0.341776,-0.363802 0.68094,-0.661458 0.7537,-0.661458 0.07276,0 -0.147341,0.297656 -0.489116,0.661458 -0.341776,0.363802 -0.68094,0.661458 -0.7537,0.661458 -0.07276,0 0.147341,-0.297656 0.489116,-0.661458 z M 252.24197,70.06553 c -0.32911,-0.419646 -0.31975,-0.429004 0.0999,-0.0999 0.25466,0.199721 0.46302,0.408081 0.46302,0.463021 0,0.217763 -0.21727,0.07761 -0.56291,-0.363124 z M 158.08405,4.8119883 c 0,-0.054941 0.20836,-0.2633001 0.46302,-0.4630209 0.41965,-0.3291072 0.42901,-0.3197489 0.0999,0.099896 -0.34565,0.4407297 -0.56292,0.5808874 -0.56292,0.3631249 z m 3.43958,-2.9104167 c 0,-0.054941 0.20836,-0.2633001 0.46303,-0.4630195 0.41964,-0.3291075 0.429,-0.3197492 0.0999,0.099895 -0.34564,0.4407297 -0.56292,0.5808874 -0.56292,0.3631247 z'),
									$elm$svg$Svg$Attributes$style('fill:#000000;fill-opacity:1;stroke-width:0.264583')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path981'),
									$elm$svg$Svg$Attributes$d('m 93.756552,195.21344 c -0.329107,-0.41964 -0.319749,-0.429 0.0999,-0.0999 0.44073,0.34564 0.580887,0.56292 0.363125,0.56292 -0.05494,0 -0.2633,-0.20836 -0.463021,-0.46303 z m -1.309192,-1.32291 -0.641433,-0.72761 0.727604,0.64144 c 0.680056,0.59951 0.84894,0.81377 0.641432,0.81377 -0.04739,0 -0.374814,-0.32742 -0.727603,-0.7276 z m 38.49176,-25.00313 -0.50403,-0.59531 0.59532,0.50403 c 0.55944,0.47366 0.71576,0.6866 0.50402,0.6866 -0.0502,0 -0.31809,-0.26789 -0.59531,-0.59532 z m -3.05132,-3.0427 c -0.32911,-0.41965 -0.31975,-0.42901 0.0999,-0.0999 0.25466,0.19972 0.46302,0.40808 0.46302,0.46302 0,0.21776 -0.21727,0.0776 -0.56292,-0.36312 z M 66.844579,137.9973 c 0,-0.3638 0.06005,-0.51263 0.133452,-0.33073 0.0734,0.1819 0.0734,0.47956 0,0.66146 -0.0734,0.1819 -0.133452,0.0331 -0.133452,-0.33073 z m 167.195781,-0.41892 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15986 0.0764,0.191 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15045,-0.0683 -0.13896,-0.34726 z m 48.44944,-1.03629 c 0.005,-0.43656 0.0647,-0.58312 0.13177,-0.32569 0.0671,0.25743 0.0627,0.61462 -0.01,0.79375 -0.0725,0.17913 -0.12736,-0.0315 -0.12197,-0.46806 z m 0.0205,-3.83646 c 0,-0.65484 0.0516,-0.92273 0.11466,-0.59531 0.0631,0.32742 0.0631,0.8632 0,1.19063 -0.0631,0.32742 -0.11466,0.0595 -0.11466,-0.59532 z m -167.26784,-0.6835 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15986 0.0764,0.191 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15045,-0.0684 -0.13896,-0.34726 z m -48.418752,-1.32292 c 0.0127,-0.30823 0.07539,-0.37092 0.159853,-0.15985 0.07643,0.19099 0.06703,0.4192 -0.02089,0.50712 -0.08792,0.0879 -0.150453,-0.0684 -0.138965,-0.34727 z M 91.938219,74.894175 c 0.341774,-0.363802 0.680939,-0.661458 0.753699,-0.661458 0.07276,0 -0.147342,0.297656 -0.489116,0.661458 -0.341774,0.363802 -0.680939,0.661459 -0.753699,0.661459 -0.07276,0 0.147342,-0.297657 0.489116,-0.661459 z'),
									$elm$svg$Svg$Attributes$style('fill:#000000;fill-opacity:1;stroke-width:0.264583')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path979'),
									$elm$svg$Svg$Attributes$d('m 66.823691,137.04921 c 0.0127,-0.30823 0.07539,-0.37092 0.159852,-0.15985 0.07643,0.19099 0.06703,0.4192 -0.02089,0.50712 -0.08792,0.0879 -0.150453,-0.0684 -0.138964,-0.34727 z M 282.51028,134.8223 c 0,-0.65484 0.0516,-0.92273 0.11466,-0.59531 0.0631,0.32742 0.0631,0.8632 0,1.19062 -0.0631,0.32742 -0.11466,0.0595 -0.11466,-0.59531 z m -28.9454,-63.433854 c -0.3291,-0.419645 -0.31975,-0.429003 0.0999,-0.0999 0.44073,0.345644 0.58089,0.562917 0.36312,0.562917 -0.0549,0 -0.2633,-0.208359 -0.46302,-0.463021 z'),
									$elm$svg$Svg$Attributes$style('fill:#000000;fill-opacity:1;stroke-width:0.264583')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path977'),
									$elm$svg$Svg$Attributes$d('m 253.59863,196.89949 c 0,-0.0549 0.20836,-0.2633 0.46302,-0.46302 0.41965,-0.32911 0.42901,-0.31975 0.0999,0.0999 -0.34564,0.44073 -0.56292,0.58089 -0.56292,0.36313 z m 5.55625,-5.55625 c 0,-0.0549 0.20836,-0.2633 0.46302,-0.46302 0.41965,-0.32911 0.42901,-0.31975 0.0999,0.0999 -0.34564,0.44073 -0.56292,0.58089 -0.56292,0.36313 z m -39.15833,-23.31573 c 0.49365,-0.50933 0.95709,-0.92604 1.02985,-0.92604 0.0728,0 -0.27161,0.41671 -0.76527,0.92604 -0.49365,0.50932 -0.95708,0.92604 -1.02984,0.92604 -0.0728,0 0.27161,-0.41672 0.76526,-0.92604 z M 66.869526,135.74834 c 0.0029,-0.58208 0.05712,-0.78905 0.120406,-0.45993 0.06329,0.32911 0.06088,0.80536 -0.0053,1.05833 -0.06622,0.25296 -0.118004,-0.0163 -0.115067,-0.5984 z m 0.0053,-3.04271 c 0,-0.65484 0.0516,-0.92273 0.114662,-0.59531 0.06306,0.32742 0.06306,0.8632 0,1.19063 -0.06306,0.32742 -0.114662,0.0595 -0.114662,-0.59532 z m 48.367574,-1.47725 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15986 0.0764,0.191 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15046,-0.0683 -0.13897,-0.34726 z m 13.34057,-28.94983 c 0.56839,-0.58208 1.09297,-1.05833 1.16573,-1.05833 0.0728,0 -0.33276,0.47625 -0.90115,1.05833 -0.56839,0.58208 -1.09296,1.05833 -1.16572,1.05833 -0.0728,0 0.33275,-0.47625 0.90114,-1.05833 z m 1.85208,-1.85208 c 0.26339,-0.29104 0.53843,-0.52917 0.61119,-0.52917 0.0728,0 -0.0832,0.23813 -0.3466,0.52917 -0.26339,0.29104 -0.53843,0.52916 -0.61119,0.52916 -0.0728,0 0.0832,-0.23812 0.3466,-0.52916 z M 89.556969,77.572404 c 0,-0.05494 0.208359,-0.2633 0.463021,-0.463021 0.419644,-0.329107 0.429002,-0.319749 0.09989,0.0999 -0.345644,0.44073 -0.562916,0.580888 -0.562916,0.363125 z'),
									$elm$svg$Svg$Attributes$style('fill:#000000;fill-opacity:1;stroke-width:0.264583')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path975'),
									$elm$svg$Svg$Attributes$d('m 157.57577,289.18463 c 0.0127,-0.30824 0.0754,-0.37092 0.15985,-0.15986 0.0764,0.191 0.067,0.41921 -0.0209,0.50713 -0.0879,0.0879 -0.15045,-0.0684 -0.13897,-0.34727 z m 0.077,-4.60817 c 0,-1.52796 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 0,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 0,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.63459,-1.5875 c 0,-0.65485 0.0516,-0.92274 0.11467,-0.59531 0.0631,0.32742 0.0631,0.8632 0,1.19062 -0.0631,0.32742 -0.11467,0.0595 -0.11467,-0.59531 z m -34.63459,-8.99583 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m -34.66042,-10.58334 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z M 157.65277,231.6598 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m -34.73742,-5.18142 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15986 0.0764,0.191 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15045,-0.0683 -0.13897,-0.34726 z m -62.090814,-29.80973 -0.504029,-0.59531 0.595313,0.50403 c 0.559445,0.47366 0.715761,0.6866 0.504028,0.6866 -0.05021,0 -0.318098,-0.26789 -0.595312,-0.59532 z m 159.304304,-0.99218 c 0.26339,-0.29105 0.53842,-0.52917 0.61118,-0.52917 0.0728,0 -0.0832,0.23812 -0.3466,0.52917 -0.26339,0.29104 -0.53842,0.52916 -0.61118,0.52916 -0.0728,0 0.0832,-0.23812 0.3466,-0.52916 z m 3.175,-3.175 c 0.56839,-0.58209 1.09297,-1.05834 1.16573,-1.05834 0.0728,0 -0.33276,0.47625 -0.90115,1.05834 -0.56839,0.58208 -1.09297,1.05833 -1.16573,1.05833 -0.0728,0 0.33276,-0.47625 0.90115,-1.05833 z M 157.65277,178.74313 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z M 157.65277,168.1598 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m -34.66042,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m -34.66042,-10.58334 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 89.88133,-7.56267 c 0.0127,-0.30823 0.0754,-0.37092 0.15985,-0.15985 0.0764,0.19099 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15046,-0.0684 -0.13897,-0.34727 z M 157.65277,136.4098 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z M 66.844579,134.29313 c 0,-0.3638 0.06005,-0.51263 0.133452,-0.33073 0.0734,0.1819 0.0734,0.47956 0,0.66146 -0.0734,0.1819 -0.133452,0.0331 -0.133452,-0.33073 z m 90.808191,-8.46666 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.13891 -0.0972,-1.38906 z m -34.66042,-10.58334 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z M 157.65277,104.6598 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 27.91419,-2.3151 c -0.3291,-0.41965 -0.31975,-0.42901 0.0999,-0.0999 0.25466,0.19972 0.46302,0.40808 0.46302,0.46302 0,0.21776 -0.21727,0.0776 -0.56292,-0.36312 z m -1.05833,-1.05834 c -0.32911,-0.41964 -0.31975,-0.429 0.0999,-0.0999 0.44073,0.34564 0.58088,0.56291 0.36312,0.56291 -0.0549,0 -0.2633,-0.20836 -0.46302,-0.46302 z m -61.51628,-7.209893 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763985 0.0534,2.014141 0,2.778125 -0.0535,0.763985 -0.0972,0.138906 -0.0972,-1.389062 z m 34.66042,0 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763985 0.0534,2.014141 0,2.778125 -0.0534,0.763985 -0.0972,0.138906 -0.0972,-1.389062 z M 157.4396,86.216524 c -0.14358,-0.377084 -0.11319,-0.407474 0.14812,-0.148114 0.18082,0.179478 0.26211,0.39297 0.18065,0.474433 -0.0815,0.08146 -0.22941,-0.06538 -0.32877,-0.326319 z m 100.7641,-10.19787 -0.50403,-0.595312 0.59532,0.504029 c 0.32742,0.277214 0.59531,0.545105 0.59531,0.595312 0,0.211733 -0.21293,0.05542 -0.6866,-0.504029 z m -2.63314,-2.645833 -1.04589,-1.124479 1.12448,1.045885 c 0.61847,0.575236 1.12448,1.081251 1.12448,1.124479 0,0.200178 -0.23041,-1.3e-4 -1.20307,-1.045885 z M 94.848636,72.280738 c 0,-0.05494 0.208359,-0.263301 0.46302,-0.463021 0.419645,-0.329108 0.429004,-0.319749 0.0999,0.0999 -0.345643,0.440729 -0.562916,0.580887 -0.562916,0.363125 z M 157.6488,40.762926 c -7.9e-4,-1.309688 0.0439,-1.878828 0.0994,-1.264754 0.0555,0.614072 0.0562,1.685634 0.002,2.38125 -0.0547,0.695614 -0.10008,0.193191 -0.10089,-1.116496 z m 34.66042,0 c -8e-4,-1.309688 0.0439,-1.878828 0.0994,-1.264754 0.0555,0.614072 0.0562,1.685634 0.002,2.38125 -0.0548,0.695614 -0.10021,0.193191 -0.10101,-1.116496 z M 157.65277,30.576467 c 0,-1.527968 0.0437,-2.153046 0.0972,-1.389062 0.0534,0.763984 0.0534,2.014141 0,2.778125 -0.0535,0.763984 -0.0972,0.138906 -0.0972,-1.389063 z m 34.66042,0 c 0,-1.527968 0.0437,-2.153046 0.0972,-1.389062 0.0534,0.763984 0.0534,2.014141 0,2.778125 -0.0534,0.763984 -0.0972,0.138906 -0.0972,-1.389063 z M 157.65277,19.993134 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778125 -0.0535,0.763984 -0.0972,0.138906 -0.0972,-1.389063 z m 34.66042,0 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778125 -0.0534,0.763984 -0.0972,0.138906 -0.0972,-1.389063 z M 157.65386,9.2775093 c 5.3e-4,-1.6007291 0.0442,-2.2214602 0.0968,-1.3794025 0.0527,0.8420576 0.0522,2.1517452 -0.001,2.9104162 -0.0533,0.758672 -0.0964,0.06972 -0.0958,-1.5310137 z m 34.65933,0.1322917 c 0,-1.5279688 0.0437,-2.1530469 0.0972,-1.3890625 0.0534,0.7639844 0.0534,2.0141405 0,2.7781245 -0.0534,0.763985 -0.0972,0.138907 -0.0972,-1.389062 z m 0,-10.5833333 c 0,-1.5279687 0.0437,-2.1530468 0.0972,-1.3890624 0.0534,0.7639843 0.0534,2.01414057 0,2.77812493 -0.0534,0.76398437 -0.0972,0.13890625 -0.0972,-1.38906253 z m 0,-10.5833327 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763984 0.0534,2.014141 0,2.778125 -0.0534,0.7639844 -0.0972,0.138906 -0.0972,-1.389062 z m -0.14389,-10.054789 c -0.0475,-1.164508 0.003,-2.117008 0.11215,-2.116666 0.10914,3.42e-4 0.19843,0.953121 0.19843,2.117288 0,1.164167 -0.0505,2.116667 -0.11214,2.116667 -0.0617,0 -0.15098,-0.95278 -0.19844,-2.117289 z'),
									$elm$svg$Svg$Attributes$style('fill:#000000;fill-opacity:1;stroke-width:0.264583')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path973'),
									$elm$svg$Svg$Attributes$d('m 157.57577,289.97838 c 0.0127,-0.30824 0.0754,-0.37092 0.15985,-0.15986 0.0764,0.191 0.067,0.41921 -0.0209,0.50713 -0.0879,0.0879 -0.15045,-0.0684 -0.13897,-0.34727 z m 0.0405,-1.96233 c 0,-0.50932 0.0545,-0.71768 0.121,-0.46302 0.0666,0.25466 0.0666,0.67138 0,0.92604 -0.0666,0.25466 -0.121,0.0463 -0.121,-0.46302 z m 0.0365,-8.73125 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76398 -0.0972,0.13891 -0.0972,-1.38906 z m 0,-10.58334 c 0,-1.52796 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 0,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z M 157.65277,247.5348 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m -34.66042,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m -34.68624,-8.99584 c 0,-0.65484 0.0516,-0.92273 0.11466,-0.59531 0.0631,0.32742 0.0631,0.8632 0,1.19062 -0.0631,0.32743 -0.11466,0.0595 -0.11466,-0.59531 z m 34.66402,-0.13229 c 0.002,-0.7276 0.0534,-0.99386 0.11419,-0.59167 0.0608,0.40218 0.0592,0.99749 -0.004,1.32291 -0.0628,0.32542 -0.11256,-0.004 -0.11057,-0.73124 z m -97.864347,-32.21302 -0.504029,-0.59531 0.595312,0.50403 c 0.559446,0.47366 0.715762,0.68659 0.504029,0.68659 -0.05021,0 -0.318098,-0.26789 -0.595312,-0.59531 z m 161.024097,-0.54581 c 0,-0.0455 0.38695,-0.43249 0.85989,-0.8599 l 0.8599,-0.7771 -0.77711,0.85989 c -0.72437,0.80155 -0.94268,0.98151 -0.94268,0.77711 z M 90.31697,191.50928 c -0.329107,-0.41965 -0.319749,-0.42901 0.0999,-0.0999 0.440729,0.34565 0.580887,0.56292 0.363126,0.56292 -0.05494,0 -0.263302,-0.20836 -0.463021,-0.46302 z m 169.89625,-1.48896 c 0,-0.0549 0.20836,-0.2633 0.46302,-0.46302 0.41964,-0.32911 0.429,-0.31975 0.0999,0.0999 -0.34564,0.44073 -0.56291,0.58089 -0.56291,0.36313 z m -102.67126,-7.64606 c 0.12878,-1.09007 0.1343,-1.09559 0.22404,-0.22405 0.0506,0.49145 -0.009,0.99436 -0.13203,1.11759 -0.12323,0.12322 -0.16463,-0.27887 -0.092,-0.89354 z m 34.71512,-0.45613 c 0,-0.3638 0.06,-0.51263 0.13345,-0.33073 0.0734,0.1819 0.0734,0.47956 0,0.66146 -0.0734,0.1819 -0.13345,0.0331 -0.13345,-0.33073 z m -34.60431,-8.46666 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m -62.94888,-5.88698 -1.04588,-1.12448 1.12448,1.04588 c 0.61846,0.57524 1.12448,1.08125 1.12448,1.12448 0,0.20018 -0.23042,-1.3e-4 -1.20308,-1.04588 z m 28.28846,-4.69636 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z M 157.65277,152.2848 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m -34.66042,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38906 z m -34.66042,-10.58334 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15304 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.38907 z m 89.88133,-1.74184 c 0.0127,-0.30822 0.0754,-0.37092 0.15985,-0.15985 0.0764,0.191 0.067,0.4192 -0.0209,0.50712 -0.0879,0.0879 -0.15046,-0.0683 -0.13897,-0.34727 z M 67.088274,128.58254 c 0.0127,-0.30822 0.07539,-0.37092 0.159853,-0.15985 0.07643,0.191 0.06703,0.4192 -0.02089,0.50712 -0.08792,0.0879 -0.150453,-0.0684 -0.138965,-0.34727 z m 90.564496,-8.04774 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0535,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38906 0.0534,0.76398 0.0534,2.01414 0,2.77812 -0.0534,0.76399 -0.0972,0.13891 -0.0972,-1.38906 z m -34.66042,-10.58333 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0535,0.76398 -0.0972,0.13891 -0.0972,-1.38906 z m 34.66042,0 c 0,-1.52797 0.0437,-2.15305 0.0972,-1.38907 0.0534,0.76399 0.0534,2.01414 0,2.77813 -0.0534,0.76398 -0.0972,0.13891 -0.0972,-1.38906 z m 28.84885,-6.68073 -0.50403,-0.59532 0.59531,0.50403 c 0.32742,0.27722 0.59531,0.54511 0.59531,0.59532 0,0.21173 -0.21293,0.0554 -0.68659,-0.50403 z m -63.50927,-3.902607 c 0,-1.527968 0.0437,-2.153046 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778129 -0.0535,0.76398 -0.0972,0.1389 -0.0972,-1.389067 z m 34.66042,0 c 0,-1.527968 0.0437,-2.153046 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778129 -0.0534,0.76398 -0.0972,0.1389 -0.0972,-1.389067 z m 26.06211,1.124477 c -0.32911,-0.41964 -0.31975,-0.429 0.0999,-0.0999 0.44073,0.34564 0.58088,0.56291 0.36312,0.56291 -0.0549,0 -0.2633,-0.20836 -0.46302,-0.46302 z M 157.65027,89.049383 c 0,-1.382447 0.0446,-1.947994 0.0991,-1.25677 0.0545,0.691224 0.0545,1.822317 0,2.513541 -0.0545,0.691224 -0.0991,0.125677 -0.0991,-1.256771 z m 34.65748,0.264584 c 0,-1.236927 0.0456,-1.742943 0.10128,-1.124479 0.0557,0.618463 0.0557,1.630494 0,2.248958 -0.0557,0.618463 -0.10128,0.112448 -0.10128,-1.124479 z M 91.012177,76.0848 c 0.263389,-0.291041 0.538421,-0.529166 0.611181,-0.529166 0.07276,0 -0.08321,0.238125 -0.346597,0.529166 -0.263389,0.291042 -0.538421,0.529167 -0.611181,0.529167 -0.07276,0 0.08321,-0.238125 0.346597,-0.529167 z m 166.133193,-1.124479 -0.50403,-0.595312 0.59531,0.504028 c 0.32743,0.277215 0.59532,0.545105 0.59532,0.595313 0,0.211733 -0.21294,0.05542 -0.6866,-0.504029 z M 94.054886,73.042092 c 0.341775,-0.363802 0.680939,-0.661458 0.7537,-0.661458 0.07276,0 -0.147342,0.297656 -0.489117,0.661458 -0.341776,0.363802 -0.68094,0.661458 -0.7537,0.661458 -0.07276,0 0.147341,-0.297656 0.489117,-0.661458 z M 157.65277,35.868134 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778125 -0.0535,0.763984 -0.0972,0.138906 -0.0972,-1.389063 z m 34.66042,0 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389062 0.0534,0.763984 0.0534,2.01414 0,2.778125 -0.0534,0.763984 -0.0972,0.138906 -0.0972,-1.389063 z M 157.65277,25.284801 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763985 0.0534,2.014141 0,2.778125 -0.0535,0.763985 -0.0972,0.138907 -0.0972,-1.389062 z m 34.66042,0 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763985 0.0534,2.014141 0,2.778125 -0.0534,0.763985 -0.0972,0.138907 -0.0972,-1.389062 z M 157.65277,14.701468 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763984 0.0534,2.014141 0,2.778125 -0.0535,0.763984 -0.0972,0.138906 -0.0972,-1.389062 z m 34.66042,0 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763984 0.0534,2.014141 0,2.778125 -0.0534,0.763984 -0.0972,0.138906 -0.0972,-1.389062 z m 0,-10.5833336 c 0,-1.5279688 0.0437,-2.1530469 0.0972,-1.3890625 0.0534,0.7639843 0.0534,2.0141406 0,2.7781249 -0.0534,0.7639844 -0.0972,0.1389063 -0.0972,-1.3890624 z m 0,-10.5833333 c 0,-1.5279687 0.0437,-2.1530468 0.0972,-1.3890624 0.0534,0.7639843 0.0534,2.0141406 0,2.7781249 -0.0534,0.7639844 -0.0972,0.1389063 -0.0972,-1.3890625 z m -21.79372,0.9584386 c 0,-0.054942 0.20836,-0.2633017 0.46302,-0.4630208 0.41964,-0.3291076 0.429,-0.3197492 0.0999,0.099895 -0.34564,0.4407292 -0.56291,0.5808872 -0.56291,0.3631261 z m 21.79372,-11.5417717 c 0,-1.527969 0.0437,-2.153047 0.0972,-1.389063 0.0534,0.763985 0.0534,2.014141 0,2.778125 -0.0534,0.763985 -0.0972,0.138907 -0.0972,-1.389062 z'),
									$elm$svg$Svg$Attributes$style('fill:#000000;fill-opacity:1;stroke-width:0.264583')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id('path971'),
									$elm$svg$Svg$Attributes$d('m 157.81947,258.42124 c 0,-20.86823 -0.0939,-33.13821 -0.25421,-33.23732 -0.13982,-0.0864 -1.65787,-0.38798 -3.37344,-0.67014 -13.41782,-2.20688 -26.47715,-6.73862 -38.4411,-13.33952 -6.30759,-3.4801 -14.42403,-9.34135 -19.976043,-14.4256 -14.930859,-13.67293 -24.826272,-31.50865 -27.751589,-50.02011 -1.466567,-9.28046 -1.179307,-20.07768 0.784254,-29.47769 0.626919,-3.00121 2.678728,-9.6385 3.986547,-12.89587 10.253999,-25.539597 33.428071,-46.140883 63.022161,-56.025439 5.65046,-1.887278 13.4454,-3.708328 21.14352,-4.939532 l 0.8599,-0.137531 V 24.25876 5.2650317 l 1.12448,-0.8998215 c 0.61846,-0.4949005 4.69635,-3.93961959 9.06198,-7.654931 4.36562,-3.7153114 9.12812,-7.7630652 10.58333,-8.9950092 1.45521,-1.231944 5.08661,-4.31769 8.06979,-6.857214 l 5.42396,-4.617317 0.13229,33.5840107 0.13229,33.5840093 0.66146,0.07749 c 2.75363,0.322585 10.46392,1.929305 14.30204,2.980349 27.06729,7.412175 49.02109,23.080974 62.63629,44.704587 9.2056,14.620305 13.70399,32.836155 12.21707,49.471945 -1.60559,17.96336 -8.73314,34.36827 -21.03683,48.41875 -12.61611,14.40728 -29.53715,25.22776 -49.20086,31.46246 -4.60363,1.45965 -11.6871,3.1799 -15.87611,3.85558 -1.4546,0.23463 -2.91261,0.48257 -3.24004,0.55098 l -0.59531,0.1244 v 18.98729 18.98729 l -6.37969,5.28318 c -3.50883,2.90576 -8.25378,6.83101 -10.54432,8.72278 -7.4376,6.14273 -14.36892,11.8773 -15.93773,13.18591 l -1.53409,1.27968 z m 0,-123.70275 c 0,-26.67023 -0.0686,-48.559896 -0.15237,-48.643698 -0.23544,-0.235442 -4.56531,1.021321 -7.72234,2.241444 -5.34491,2.06569 -10.95294,5.218615 -15.8055,8.886113 -2.55581,1.931641 -8.32282,7.919061 -10.18585,10.575141 -5.96276,8.50094 -8.73987,16.99617 -8.72701,26.69606 0.008,6.08373 0.84358,10.52238 3.03631,16.13027 5.1251,13.10743 16.28545,23.96622 30.82551,29.99257 3.0838,1.27813 8.22458,2.92652 8.53281,2.73605 0.10914,-0.0674 0.19844,-21.94372 0.19844,-48.61395 z m 37.69079,47.97053 c 19.82419,-6.19669 34.38177,-21.05965 38.00704,-38.80433 0.68789,-3.367 0.94302,-10.12046 0.52243,-13.82872 -1.988,-17.52763 -15.14119,-33.53791 -33.75464,-41.086708 -3.04611,-1.235371 -7.03166,-2.565712 -7.68657,-2.565712 -0.33318,0 -0.38322,6.340023 -0.38322,48.55104 0,39.46264 0.0639,48.55104 0.34125,48.55104 0.18768,0 1.51686,-0.36747 2.95371,-0.81661 z')
								]),
							_List_Nil)
						]))
				]))
		]));
var $mdgriffith$elm_ui$Internal$Model$Button = {$: 8};
var $mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 2, a: a};
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 0};
var $mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 4) && (attr.b.$ === 11)) && (!attr.b.a)) {
		var _v1 = attr.b;
		var _v2 = _v1.a;
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$focusDefault = function (attrs) {
	return A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, attrs) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass('focusable');
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $mdgriffith$elm_ui$Element$Events$onClick = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Events$onClick);
var $mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $mdgriffith$elm_ui$Element$Input$onKey = F2(
	function (desiredCode, msg) {
		var decode = function (code) {
			return _Utils_eq(code, desiredCode) ? $elm$json$Json$Decode$succeed(msg) : $elm$json$Json$Decode$fail('Not the enter key');
		};
		var isKey = A2(
			$elm$json$Json$Decode$andThen,
			decode,
			A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
		return $mdgriffith$elm_ui$Internal$Model$Attr(
			A2(
				$elm$html$Html$Events$preventDefaultOn,
				'keyup',
				A2(
					$elm$json$Json$Decode$map,
					function (fired) {
						return _Utils_Tuple2(fired, true);
					},
					isKey)));
	});
var $mdgriffith$elm_ui$Element$Input$onEnter = function (msg) {
	return A2($mdgriffith$elm_ui$Element$Input$onKey, $mdgriffith$elm_ui$Element$Input$enter, msg);
};
var $mdgriffith$elm_ui$Internal$Flag$cursor = $mdgriffith$elm_ui$Internal$Flag$flag(21);
var $mdgriffith$elm_ui$Element$pointer = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.cD);
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _v0) {
		var onPress = _v0.c4;
		var label = _v0._;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aC + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.B + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.di + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bD)))))),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$pointer,
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Button),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Internal$Model$Attr(
											$elm$html$Html$Attributes$tabindex(0)),
										function () {
											if (onPress.$ === 1) {
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Internal$Model$Attr(
														$elm$html$Html$Attributes$disabled(true)),
													attrs);
											} else {
												var msg = onPress.a;
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Element$Events$onClick(msg),
													A2(
														$elm$core$List$cons,
														$mdgriffith$elm_ui$Element$Input$onEnter(msg),
														attrs));
											}
										}()))))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $mdgriffith$elm_ui$Internal$Model$CenterX = 1;
var $mdgriffith$elm_ui$Element$centerX = $mdgriffith$elm_ui$Internal$Model$AlignX(1);
var $mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$CenterY = 1;
var $mdgriffith$elm_ui$Element$centerY = $mdgriffith$elm_ui$Internal$Model$AlignY(1);
var $mdgriffith$elm_ui$Internal$Model$unstyled = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Unstyled, $elm$core$Basics$always);
var $mdgriffith$elm_ui$Element$html = $mdgriffith$elm_ui$Internal$Model$unstyled;
var $author$project$Main$viewCountMode = F2(
	function (svg, action) {
		return A2(
			$mdgriffith$elm_ui$Element$Input$button,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width(
					$mdgriffith$elm_ui$Element$fillPortion(1)),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					A2($mdgriffith$elm_ui$Element$paddingXY, 25, 25)
				]),
			{
				_: A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$centerY]),
					$mdgriffith$elm_ui$Element$html(svg)),
				c4: $elm$core$Maybe$Just(action)
			});
	});
var $mdgriffith$elm_ui$Element$Border$widthXY = F2(
	function (x, y) {
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$borderWidth,
			A5(
				$mdgriffith$elm_ui$Internal$Model$BorderWidth,
				'b-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var $mdgriffith$elm_ui$Element$Border$widthEach = function (_v0) {
	var bottom = _v0.bc;
	var top = _v0.b$;
	var left = _v0.bv;
	var right = _v0.bT;
	return (_Utils_eq(top, bottom) && _Utils_eq(left, right)) ? (_Utils_eq(top, right) ? $mdgriffith$elm_ui$Element$Border$width(top) : A2($mdgriffith$elm_ui$Element$Border$widthXY, left, top)) : A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left))))))),
			top,
			right,
			bottom,
			left));
};
var $author$project$Main$viewCountModes = function (accent) {
	return A2(
		$mdgriffith$elm_ui$Element$row,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$Border$solid,
				$mdgriffith$elm_ui$Element$Border$widthEach(
				{bc: 0, bv: 0, bT: 0, b$: 8}),
				$mdgriffith$elm_ui$Element$Border$color(accent.as)
			]),
		_List_fromArray(
			[
				A2(
				$author$project$Main$viewCountMode,
				$author$project$Icons$health,
				$author$project$Main$SetMode(0)),
				A2(
				$author$project$Main$viewCountMode,
				$author$project$Icons$poison,
				$author$project$Main$SetMode(1)),
				A2(
				$author$project$Main$viewCountMode,
				$author$project$Icons$commander,
				$author$project$Main$SetMode(2)),
				A2(
				$author$project$Main$viewCountMode,
				$author$project$Icons$mana,
				$author$project$Main$SetMode(3)),
				A2(
				$author$project$Main$viewCountMode,
				$author$project$Icons$counters,
				$author$project$Main$SetMode(4))
			]));
};
var $author$project$Main$EditCommanderName = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Main$SelectCommander = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Main$UpdateCommanderDamage = F3(
	function (a, b, c) {
		return {$: 6, a: a, b: b, c: c};
	});
var $author$project$Utils$listUnwrapMaybe = function (listMaybes) {
	if (listMaybes.b) {
		if (!listMaybes.a.$) {
			var head = listMaybes.a.a;
			var tail = listMaybes.b;
			var maybeTail = $author$project$Utils$listUnwrapMaybe(tail);
			return A2(
				$elm$core$Maybe$map,
				function (t) {
					return A2($elm$core$List$cons, head, t);
				},
				maybeTail);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	} else {
		return $elm$core$Maybe$Just(_List_Nil);
	}
};
var $author$project$Main$themeCommander = {
	co: A3($mdgriffith$elm_ui$Element$rgb, 0.9, 0.9, 0.9),
	bd: A3($mdgriffith$elm_ui$Element$rgb, 0.2, 0.5, 0.9),
	aw: A3($mdgriffith$elm_ui$Element$rgb, 0.2, 0.5, 0.8)
};
var $mdgriffith$elm_ui$Element$Input$HiddenLabel = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Element$Input$labelHidden = $mdgriffith$elm_ui$Element$Input$HiddenLabel;
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $mdgriffith$elm_ui$Element$Events$onFocus = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Events$onFocus);
var $mdgriffith$elm_ui$Internal$Model$Paragraph = {$: 9};
var $mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Flag$spacing = $mdgriffith$elm_ui$Internal$Flag$flag(3);
var $mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y)));
	});
var $mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			$mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2($mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var $mdgriffith$elm_ui$Element$paragraph = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asParagraph,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Paragraph),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$spacing(5),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Element$Input$Placeholder = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$Input$placeholder = $mdgriffith$elm_ui$Element$Input$Placeholder;
var $mdgriffith$elm_ui$Element$rgba = $mdgriffith$elm_ui$Internal$Model$Rgba;
var $mdgriffith$elm_ui$Element$Input$TextInputNode = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$Input$TextArea = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$LivePolite = {$: 6};
var $mdgriffith$elm_ui$Element$Region$announce = $mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$LivePolite);
var $mdgriffith$elm_ui$Element$Input$applyLabel = F3(
	function (attrs, label, input) {
		if (label.$ === 1) {
			var labelText = label.a;
			return A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asColumn,
				$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
				attrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[input])));
		} else {
			var position = label.a;
			var labelAttrs = label.b;
			var labelChild = label.c;
			var labelElement = A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				labelAttrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[labelChild])));
			switch (position) {
				case 2:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
				case 3:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				case 0:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				default:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
			}
		}
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $mdgriffith$elm_ui$Element$Input$autofill = A2(
	$elm$core$Basics$composeL,
	$mdgriffith$elm_ui$Internal$Model$Attr,
	$elm$html$Html$Attributes$attribute('autocomplete'));
var $mdgriffith$elm_ui$Internal$Model$Behind = 5;
var $mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$createNearby = F2(
	function (loc, element) {
		if (element.$ === 3) {
			return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
		} else {
			return A2($mdgriffith$elm_ui$Internal$Model$Nearby, loc, element);
		}
	});
var $mdgriffith$elm_ui$Element$behindContent = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 5, element);
};
var $mdgriffith$elm_ui$Internal$Model$MoveY = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$TransformComponent = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$moveY = $mdgriffith$elm_ui$Internal$Flag$flag(26);
var $mdgriffith$elm_ui$Element$moveUp = function (y) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$moveY,
		$mdgriffith$elm_ui$Internal$Model$MoveY(-y));
};
var $mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding = function (attrs) {
	var gatherSpacing = F2(
		function (attr, found) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v2 = attr.b;
				var x = _v2.b;
				var y = _v2.c;
				if (found.$ === 1) {
					return $elm$core$Maybe$Just(y);
				} else {
					return found;
				}
			} else {
				return found;
			}
		});
	var _v0 = A3($elm$core$List$foldr, gatherSpacing, $elm$core$Maybe$Nothing, attrs);
	if (_v0.$ === 1) {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	} else {
		var vSpace = _v0.a;
		return $mdgriffith$elm_ui$Element$moveUp(
			$elm$core$Basics$floor(vSpace / 2));
	}
};
var $mdgriffith$elm_ui$Element$clip = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.cx);
var $mdgriffith$elm_ui$Element$Background$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$bgColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'background-color',
			clr));
};
var $mdgriffith$elm_ui$Element$Input$darkGrey = A3($mdgriffith$elm_ui$Element$rgb, 186 / 255, 189 / 255, 182 / 255);
var $mdgriffith$elm_ui$Element$Input$defaultTextPadding = A2($mdgriffith$elm_ui$Element$paddingXY, 12, 12);
var $mdgriffith$elm_ui$Element$Input$white = A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var $mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Input$defaultTextPadding,
		$mdgriffith$elm_ui$Element$Border$rounded(3),
		$mdgriffith$elm_ui$Element$Border$color($mdgriffith$elm_ui$Element$Input$darkGrey),
		$mdgriffith$elm_ui$Element$Background$color($mdgriffith$elm_ui$Element$Input$white),
		$mdgriffith$elm_ui$Element$Border$width(1),
		$mdgriffith$elm_ui$Element$spacing(5),
		$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
		$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink)
	]);
var $mdgriffith$elm_ui$Element$Input$getHeight = function (attr) {
	if (attr.$ === 8) {
		var h = attr.a;
		return $elm$core$Maybe$Just(h);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Label = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute = function (label) {
	if (label.$ === 1) {
		var textLabel = label.a;
		return $mdgriffith$elm_ui$Internal$Model$Describe(
			$mdgriffith$elm_ui$Internal$Model$Label(textLabel));
	} else {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	}
};
var $mdgriffith$elm_ui$Internal$Model$InFront = 4;
var $mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 4, element);
};
var $mdgriffith$elm_ui$Element$Input$isConstrained = function (len) {
	isConstrained:
	while (true) {
		switch (len.$) {
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return true;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isConstrained;
			default:
				var l = len.b;
				return true;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isHiddenLabel = function (label) {
	if (label.$ === 1) {
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$isStacked = function (label) {
	if (!label.$) {
		var loc = label.a;
		switch (loc) {
			case 0:
				return false;
			case 1:
				return false;
			case 2:
				return true;
			default:
				return true;
		}
	} else {
		return true;
	}
};
var $mdgriffith$elm_ui$Element$Input$negateBox = function (box) {
	return {bc: -box.bc, bv: -box.bv, bT: -box.bT, b$: -box.b$};
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left)))))));
	});
var $mdgriffith$elm_ui$Element$paddingEach = function (_v0) {
	var top = _v0.b$;
	var right = _v0.bT;
	var bottom = _v0.bc;
	var left = _v0.bv;
	return (_Utils_eq(top, right) && (_Utils_eq(top, bottom) && _Utils_eq(top, left))) ? A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + $elm$core$String$fromInt(top),
			top,
			top,
			top,
			top)) : A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			A4($mdgriffith$elm_ui$Internal$Model$paddingName, top, right, bottom, left),
			top,
			right,
			bottom,
			left));
};
var $mdgriffith$elm_ui$Element$Input$isFill = function (len) {
	isFill:
	while (true) {
		switch (len.$) {
			case 2:
				return true;
			case 1:
				return false;
			case 0:
				return false;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isPixel = function (len) {
	isPixel:
	while (true) {
		switch (len.$) {
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return false;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
		}
	}
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $mdgriffith$elm_ui$Element$Input$redistributeOver = F4(
	function (isMultiline, stacked, attr, els) {
		switch (attr.$) {
			case 9:
				return _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					});
			case 7:
				var width = attr.a;
				return $mdgriffith$elm_ui$Element$Input$isFill(width) ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b),
						h: A2($elm$core$List$cons, attr, els.h),
						a: A2($elm$core$List$cons, attr, els.a)
					}) : (stacked ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b)
					}) : _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					}));
			case 8:
				var height = attr.a;
				return (!stacked) ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b),
						a: A2($elm$core$List$cons, attr, els.a)
					}) : ($mdgriffith$elm_ui$Element$Input$isFill(height) ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b),
						a: A2($elm$core$List$cons, attr, els.a)
					}) : ($mdgriffith$elm_ui$Element$Input$isPixel(height) ? _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					}) : _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					})));
			case 6:
				return _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b)
					});
			case 5:
				return _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b)
					});
			case 4:
				switch (attr.b.$) {
					case 5:
						var _v1 = attr.b;
						return _Utils_update(
							els,
							{
								b: A2($elm$core$List$cons, attr, els.b),
								h: A2($elm$core$List$cons, attr, els.h),
								a: A2($elm$core$List$cons, attr, els.a),
								ah: A2($elm$core$List$cons, attr, els.ah)
							});
					case 7:
						var cls = attr.a;
						var _v2 = attr.b;
						var pad = _v2.a;
						var t = _v2.b;
						var r = _v2.c;
						var b = _v2.d;
						var l = _v2.e;
						if (isMultiline) {
							return _Utils_update(
								els,
								{
									m: A2($elm$core$List$cons, attr, els.m),
									a: A2($elm$core$List$cons, attr, els.a)
								});
						} else {
							var reducedVerticalPadding = $mdgriffith$elm_ui$Element$paddingEach(
								{
									bc: b - A2($elm$core$Basics$min, t, b),
									bv: l,
									bT: r,
									b$: t - A2($elm$core$Basics$min, t, b)
								});
							var newLineHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'line-height',
									'calc(1.0em + ' + ($elm$core$String$fromInt(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'height',
									'calc(1.0em + ' + ($elm$core$String$fromInt(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							return _Utils_update(
								els,
								{
									m: A2($elm$core$List$cons, attr, els.m),
									h: A2(
										$elm$core$List$cons,
										newHeight,
										A2($elm$core$List$cons, newLineHeight, els.h)),
									a: A2($elm$core$List$cons, reducedVerticalPadding, els.a)
								});
						}
					case 6:
						var _v3 = attr.b;
						return _Utils_update(
							els,
							{
								m: A2($elm$core$List$cons, attr, els.m),
								a: A2($elm$core$List$cons, attr, els.a)
							});
					case 10:
						return _Utils_update(
							els,
							{
								m: A2($elm$core$List$cons, attr, els.m),
								a: A2($elm$core$List$cons, attr, els.a)
							});
					case 2:
						return _Utils_update(
							els,
							{
								b: A2($elm$core$List$cons, attr, els.b)
							});
					case 1:
						var _v4 = attr.b;
						return _Utils_update(
							els,
							{
								b: A2($elm$core$List$cons, attr, els.b)
							});
					default:
						var flag = attr.a;
						var cls = attr.b;
						return _Utils_update(
							els,
							{
								a: A2($elm$core$List$cons, attr, els.a)
							});
				}
			case 0:
				return els;
			case 1:
				var a = attr.a;
				return _Utils_update(
					els,
					{
						h: A2($elm$core$List$cons, attr, els.h)
					});
			case 2:
				return _Utils_update(
					els,
					{
						h: A2($elm$core$List$cons, attr, els.h)
					});
			case 3:
				return _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					});
			default:
				return _Utils_update(
					els,
					{
						h: A2($elm$core$List$cons, attr, els.h)
					});
		}
	});
var $mdgriffith$elm_ui$Element$Input$redistribute = F3(
	function (isMultiline, stacked, attrs) {
		return function (redist) {
			return {
				m: $elm$core$List$reverse(redist.m),
				b: $elm$core$List$reverse(redist.b),
				h: $elm$core$List$reverse(redist.h),
				a: $elm$core$List$reverse(redist.a),
				ah: $elm$core$List$reverse(redist.ah)
			};
		}(
			A3(
				$elm$core$List$foldl,
				A2($mdgriffith$elm_ui$Element$Input$redistributeOver, isMultiline, stacked),
				{m: _List_Nil, b: _List_Nil, h: _List_Nil, a: _List_Nil, ah: _List_Nil},
				attrs));
	});
var $mdgriffith$elm_ui$Element$Input$renderBox = function (_v0) {
	var top = _v0.b$;
	var right = _v0.bT;
	var bottom = _v0.bc;
	var left = _v0.bv;
	return $elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px'))))));
};
var $mdgriffith$elm_ui$Internal$Model$Transparency = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$transparency = $mdgriffith$elm_ui$Internal$Flag$flag(0);
var $mdgriffith$elm_ui$Element$alpha = function (o) {
	var transparency = function (x) {
		return 1 - x;
	}(
		A2(
			$elm$core$Basics$min,
			1.0,
			A2($elm$core$Basics$max, 0.0, o)));
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2(
			$mdgriffith$elm_ui$Internal$Model$Transparency,
			'transparency-' + $mdgriffith$elm_ui$Internal$Model$floatClass(transparency),
			transparency));
};
var $mdgriffith$elm_ui$Element$Input$charcoal = A3($mdgriffith$elm_ui$Element$rgb, 136 / 255, 138 / 255, 133 / 255);
var $mdgriffith$elm_ui$Element$Input$renderPlaceholder = F3(
	function (_v0, forPlaceholder, on) {
		var placeholderAttrs = _v0.a;
		var placeholderEl = _v0.b;
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_Utils_ap(
				forPlaceholder,
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$color($mdgriffith$elm_ui$Element$Input$charcoal),
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.bD + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.da)),
							$mdgriffith$elm_ui$Element$clip,
							$mdgriffith$elm_ui$Element$Border$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$Background$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$alpha(
							on ? 1 : 0)
						]),
					placeholderAttrs)),
			placeholderEl);
	});
var $mdgriffith$elm_ui$Element$scrollbarY = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.dh);
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $mdgriffith$elm_ui$Element$Input$spellcheck = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$spellcheck);
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $mdgriffith$elm_ui$Element$Input$value = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$value);
var $mdgriffith$elm_ui$Element$Input$textHelper = F3(
	function (textInput, attrs, textOptions) {
		var withDefaults = _Utils_ap($mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle, attrs);
		var redistributed = A3(
			$mdgriffith$elm_ui$Element$Input$redistribute,
			_Utils_eq(textInput.k, $mdgriffith$elm_ui$Element$Input$TextArea),
			$mdgriffith$elm_ui$Element$Input$isStacked(textOptions._),
			withDefaults);
		var onlySpacing = function (attr) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v9 = attr.b;
				return true;
			} else {
				return false;
			}
		};
		var heightConstrained = function () {
			var _v7 = textInput.k;
			if (!_v7.$) {
				var inputType = _v7.a;
				return false;
			} else {
				return A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$mdgriffith$elm_ui$Element$Input$isConstrained,
						$elm$core$List$head(
							$elm$core$List$reverse(
								A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Element$Input$getHeight, withDefaults)))));
			}
		}();
		var getPadding = function (attr) {
			if ((attr.$ === 4) && (attr.b.$ === 7)) {
				var cls = attr.a;
				var _v6 = attr.b;
				var pad = _v6.a;
				var t = _v6.b;
				var r = _v6.c;
				var b = _v6.d;
				var l = _v6.e;
				return $elm$core$Maybe$Just(
					{
						bc: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(b - 3)),
						bv: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(l - 3)),
						bT: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(r - 3)),
						b$: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(t - 3))
					});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		var parentPadding = A2(
			$elm$core$Maybe$withDefault,
			{bc: 0, bv: 0, bT: 0, b$: 0},
			$elm$core$List$head(
				$elm$core$List$reverse(
					A2($elm$core$List$filterMap, getPadding, withDefaults))));
		var inputElement = A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			function () {
				var _v3 = textInput.k;
				if (!_v3.$) {
					var inputType = _v3.a;
					return $mdgriffith$elm_ui$Internal$Model$NodeName('input');
				} else {
					return $mdgriffith$elm_ui$Internal$Model$NodeName('textarea');
				}
			}(),
			_Utils_ap(
				function () {
					var _v4 = textInput.k;
					if (!_v4.$) {
						var inputType = _v4.a;
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Internal$Model$Attr(
								$elm$html$Html$Attributes$type_(inputType)),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.c_)
							]);
					} else {
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Element$clip,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cW),
								$mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding(withDefaults),
								$mdgriffith$elm_ui$Element$paddingEach(parentPadding),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2(
									$elm$html$Html$Attributes$style,
									'margin',
									$mdgriffith$elm_ui$Element$Input$renderBox(
										$mdgriffith$elm_ui$Element$Input$negateBox(parentPadding)))),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2($elm$html$Html$Attributes$style, 'box-sizing', 'content-box'))
							]);
					}
				}(),
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Input$value(textOptions.b_),
							$mdgriffith$elm_ui$Internal$Model$Attr(
							$elm$html$Html$Events$onInput(textOptions.ab)),
							$mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(textOptions._),
							$mdgriffith$elm_ui$Element$Input$spellcheck(textInput.s),
							A2(
							$elm$core$Maybe$withDefault,
							$mdgriffith$elm_ui$Internal$Model$NoAttribute,
							A2($elm$core$Maybe$map, $mdgriffith$elm_ui$Element$Input$autofill, textInput.p))
						]),
					redistributed.h)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil));
		var wrappedInput = function () {
			var _v0 = textInput.k;
			if (_v0.$ === 1) {
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					_Utils_ap(
						(heightConstrained ? $elm$core$List$cons($mdgriffith$elm_ui$Element$scrollbarY) : $elm$core$Basics$identity)(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.bm),
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cZ)
								])),
						redistributed.a),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[
								A4(
								$mdgriffith$elm_ui$Internal$Model$element,
								$mdgriffith$elm_ui$Internal$Model$asParagraph,
								$mdgriffith$elm_ui$Internal$Model$div,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
										A2(
											$elm$core$List$cons,
											$mdgriffith$elm_ui$Element$inFront(inputElement),
											A2(
												$elm$core$List$cons,
												$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cY),
												redistributed.ah)))),
								$mdgriffith$elm_ui$Internal$Model$Unkeyed(
									function () {
										if (textOptions.b_ === '') {
											var _v1 = textOptions.ac;
											if (_v1.$ === 1) {
												return _List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('\u00A0')
													]);
											} else {
												var place = _v1.a;
												return _List_fromArray(
													[
														A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, _List_Nil, textOptions.b_ === '')
													]);
											}
										} else {
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Internal$Model$unstyled(
													A2(
														$elm$html$Html$span,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.cX)
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(textOptions.b_ + '\u00A0')
															])))
												]);
										}
									}()))
							])));
			} else {
				var inputType = _v0.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						A2(
							$elm$core$List$cons,
							A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.bm),
							$elm$core$List$concat(
								_List_fromArray(
									[
										redistributed.a,
										function () {
										var _v2 = textOptions.ac;
										if (_v2.$ === 1) {
											return _List_Nil;
										} else {
											var place = _v2.a;
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Element$behindContent(
													A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, redistributed.m, textOptions.b_ === ''))
												]);
										}
									}()
									])))),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[inputElement])));
			}
		}();
		return A3(
			$mdgriffith$elm_ui$Element$Input$applyLabel,
			A2(
				$elm$core$List$cons,
				A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.cE),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$Input$isHiddenLabel(textOptions._) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Element$spacing(5),
					A2($elm$core$List$cons, $mdgriffith$elm_ui$Element$Region$announce, redistributed.b))),
			textOptions._,
			wrappedInput);
	});
var $mdgriffith$elm_ui$Element$Input$text = $mdgriffith$elm_ui$Element$Input$textHelper(
	{
		p: $elm$core$Maybe$Nothing,
		s: false,
		k: $mdgriffith$elm_ui$Element$Input$TextInputNode('text')
	});
var $mdgriffith$elm_ui$Element$Border$roundEach = function (_v0) {
	var topLeft = _v0.aJ;
	var topRight = _v0.aK;
	var bottomLeft = _v0.au;
	var bottomRight = _v0.av;
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + ($elm$core$String$fromInt(topLeft) + ('-' + ($elm$core$String$fromInt(topRight) + ($elm$core$String$fromInt(bottomLeft) + ('-' + $elm$core$String$fromInt(bottomRight)))))),
			'border-radius',
			$elm$core$String$fromInt(topLeft) + ('px ' + ($elm$core$String$fromInt(topRight) + ('px ' + ($elm$core$String$fromInt(bottomRight) + ('px ' + ($elm$core$String$fromInt(bottomLeft) + 'px'))))))));
};
var $mdgriffith$elm_ui$Internal$Model$Hover = 1;
var $mdgriffith$elm_ui$Internal$Model$PseudoSelector = F2(
	function (a, b) {
		return {$: 11, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$hover = $mdgriffith$elm_ui$Internal$Flag$flag(33);
var $mdgriffith$elm_ui$Internal$Model$Empty = {$: 3};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $mdgriffith$elm_ui$Internal$Model$map = F2(
	function (fn, el) {
		switch (el.$) {
			case 1:
				var styled = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						cR: F2(
							function (add, context) {
								return A2(
									$elm$virtual_dom$VirtualDom$map,
									fn,
									A2(styled.cR, add, context));
							}),
						du: styled.du
					});
			case 0:
				var html = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A2(
						$elm$core$Basics$composeL,
						$elm$virtual_dom$VirtualDom$map(fn),
						html));
			case 2:
				var str = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Text(str);
			default:
				return $mdgriffith$elm_ui$Internal$Model$Empty;
		}
	});
var $elm$virtual_dom$VirtualDom$mapAttribute = _VirtualDom_mapAttribute;
var $mdgriffith$elm_ui$Internal$Model$mapAttrFromStyle = F2(
	function (fn, attr) {
		switch (attr.$) {
			case 0:
				return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
			case 2:
				var description = attr.a;
				return $mdgriffith$elm_ui$Internal$Model$Describe(description);
			case 6:
				var x = attr.a;
				return $mdgriffith$elm_ui$Internal$Model$AlignX(x);
			case 5:
				var y = attr.a;
				return $mdgriffith$elm_ui$Internal$Model$AlignY(y);
			case 7:
				var x = attr.a;
				return $mdgriffith$elm_ui$Internal$Model$Width(x);
			case 8:
				var x = attr.a;
				return $mdgriffith$elm_ui$Internal$Model$Height(x);
			case 3:
				var x = attr.a;
				var y = attr.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Class, x, y);
			case 4:
				var flag = attr.a;
				var style = attr.b;
				return A2($mdgriffith$elm_ui$Internal$Model$StyleClass, flag, style);
			case 9:
				var location = attr.a;
				var elem = attr.b;
				return A2(
					$mdgriffith$elm_ui$Internal$Model$Nearby,
					location,
					A2($mdgriffith$elm_ui$Internal$Model$map, fn, elem));
			case 1:
				var htmlAttr = attr.a;
				return $mdgriffith$elm_ui$Internal$Model$Attr(
					A2($elm$virtual_dom$VirtualDom$mapAttribute, fn, htmlAttr));
			default:
				var fl = attr.a;
				var trans = attr.b;
				return A2($mdgriffith$elm_ui$Internal$Model$TransformComponent, fl, trans);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$removeNever = function (style) {
	return A2($mdgriffith$elm_ui$Internal$Model$mapAttrFromStyle, $elm$core$Basics$never, style);
};
var $mdgriffith$elm_ui$Internal$Model$unwrapDecsHelper = F2(
	function (attr, _v0) {
		var styles = _v0.a;
		var trans = _v0.b;
		var _v1 = $mdgriffith$elm_ui$Internal$Model$removeNever(attr);
		switch (_v1.$) {
			case 4:
				var style = _v1.b;
				return _Utils_Tuple2(
					A2($elm$core$List$cons, style, styles),
					trans);
			case 10:
				var flag = _v1.a;
				var component = _v1.b;
				return _Utils_Tuple2(
					styles,
					A2($mdgriffith$elm_ui$Internal$Model$composeTransformation, trans, component));
			default:
				return _Utils_Tuple2(styles, trans);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$unwrapDecorations = function (attrs) {
	var _v0 = A3(
		$elm$core$List$foldl,
		$mdgriffith$elm_ui$Internal$Model$unwrapDecsHelper,
		_Utils_Tuple2(_List_Nil, $mdgriffith$elm_ui$Internal$Model$Untransformed),
		attrs);
	var styles = _v0.a;
	var transform = _v0.b;
	return A2(
		$elm$core$List$cons,
		$mdgriffith$elm_ui$Internal$Model$Transform(transform),
		styles);
};
var $mdgriffith$elm_ui$Element$mouseOver = function (decs) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$hover,
		A2(
			$mdgriffith$elm_ui$Internal$Model$PseudoSelector,
			1,
			$mdgriffith$elm_ui$Internal$Model$unwrapDecorations(decs)));
};
var $mdgriffith$elm_ui$Internal$Model$boxShadowClass = function (shadow) {
	return $elm$core$String$concat(
		_List_fromArray(
			[
				shadow.bu ? 'box-inset' : 'box-',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.c2.a) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.c2.b) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.cp) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.dl) + 'px',
				$mdgriffith$elm_ui$Internal$Model$formatColorClass(shadow.cB)
			]));
};
var $mdgriffith$elm_ui$Internal$Flag$shadows = $mdgriffith$elm_ui$Internal$Flag$flag(19);
var $mdgriffith$elm_ui$Element$Border$shadow = function (almostShade) {
	var shade = {cp: almostShade.cp, cB: almostShade.cB, bu: false, c2: almostShade.c2, dl: almostShade.dl};
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$shadows,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			$mdgriffith$elm_ui$Internal$Model$boxShadowClass(shade),
			'box-shadow',
			$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(shade)));
};
var $author$project$Counter$viewCounterButton = F4(
	function (theme, attrs, msg, label) {
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					A2($mdgriffith$elm_ui$Element$paddingXY, 10, 10)
				]),
			A2(
				$mdgriffith$elm_ui$Element$Input$button,
				_Utils_ap(
					attrs,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Background$color(theme.bd),
							$mdgriffith$elm_ui$Element$Font$color(
							A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1)),
							$mdgriffith$elm_ui$Element$Font$size(48),
							$mdgriffith$elm_ui$Element$Border$solid,
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$centerX,
							$mdgriffith$elm_ui$Element$centerY,
							$mdgriffith$elm_ui$Element$mouseOver(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$Background$color(theme.aw),
									$mdgriffith$elm_ui$Element$Border$color(
									A3($mdgriffith$elm_ui$Element$rgb, 0, 0, 0)),
									$mdgriffith$elm_ui$Element$Border$shadow(
									{
										cp: 5.0,
										cB: A3($mdgriffith$elm_ui$Element$rgb, 0.2, 0.2, 0.2),
										c2: _Utils_Tuple2(1.0, 1.0),
										dl: 0.2
									})
								]))
						])),
				{
					_: A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$centerY]),
						$mdgriffith$elm_ui$Element$text(label)),
					c4: $elm$core$Maybe$Just(msg)
				}));
	});
var $author$project$Counter$viewCounterLeftButtons = F3(
	function (theme, key, change) {
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width(
					$mdgriffith$elm_ui$Element$fillPortion(1)),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
				]),
			_List_fromArray(
				[
					A4(
					$author$project$Counter$viewCounterButton,
					theme,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Border$roundEach(
							{au: 0, av: 0, aJ: 40, aK: 0})
						]),
					A2(change, key, 1),
					'+1'),
					A4(
					$author$project$Counter$viewCounterButton,
					theme,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Border$roundEach(
							{au: 40, av: 0, aJ: 0, aK: 0})
						]),
					A2(change, key, -1),
					'-1')
				]));
	});
var $author$project$Counter$viewCounterRightButtons = F3(
	function (theme, key, change) {
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width(
					$mdgriffith$elm_ui$Element$fillPortion(1)),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
				]),
			_List_fromArray(
				[
					A4(
					$author$project$Counter$viewCounterButton,
					theme,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Border$roundEach(
							{au: 0, av: 0, aJ: 0, aK: 40})
						]),
					A2(change, key, 5),
					'+5'),
					A4(
					$author$project$Counter$viewCounterButton,
					theme,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Border$roundEach(
							{au: 0, av: 40, aJ: 0, aK: 0})
						]),
					A2(change, key, -5),
					'-5')
				]));
	});
var $author$project$Counter$viewMultiCounter = F2(
	function (theme, data) {
		var counterRow = function (_v0) {
			var key = _v0.E;
			var name = _v0.bA;
			var stat = _v0.bW;
			var placeholder = _v0.ac;
			return A2(
				$mdgriffith$elm_ui$Element$row,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
					]),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Element$Input$text,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$Events$onFocus(
								data.bF(key)),
								$mdgriffith$elm_ui$Element$Border$color(
								A4($mdgriffith$elm_ui$Element$rgba, 1, 1, 1, 0)),
								$mdgriffith$elm_ui$Element$Font$size(32)
							]),
						{
							_: $mdgriffith$elm_ui$Element$Input$labelHidden(placeholder),
							ab: data.bE(key),
							ac: $elm$core$Maybe$Just(
								A2(
									$mdgriffith$elm_ui$Element$Input$placeholder,
									_List_Nil,
									$mdgriffith$elm_ui$Element$text(placeholder))),
							b_: name
						}),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$alignRight,
								$mdgriffith$elm_ui$Element$Font$size(48)
							]),
						$mdgriffith$elm_ui$Element$text(stat))
					]));
		};
		var rows = A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
				]),
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$centerX,
							A2($mdgriffith$elm_ui$Element$paddingXY, 30, 30),
							$mdgriffith$elm_ui$Element$Font$size(32)
						]),
					A2(
						$mdgriffith$elm_ui$Element$paragraph,
						_List_fromArray(
							[$mdgriffith$elm_ui$Element$centerX]),
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$text(data._)
							]))),
					A2(
					$mdgriffith$elm_ui$Element$column,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							A2($mdgriffith$elm_ui$Element$paddingXY, 30, 30)
						]),
					A2($elm$core$List$map, counterRow, data.bk))
				]));
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
				]),
			_List_fromArray(
				[
					A3($author$project$Counter$viewCounterLeftButtons, theme, data.E, data.a$),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$width(
							$mdgriffith$elm_ui$Element$fillPortion(2)),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$Border$color(theme.aw),
							$mdgriffith$elm_ui$Element$Border$widthEach(
							{bc: 0, bv: 8, bT: 8, b$: 0})
						]),
					rows),
					A3($author$project$Counter$viewCounterRightButtons, theme, data.E, data.a$)
				]));
	});
var $author$project$Main$viewCommanderCounter = F2(
	function (model, selectedPlayer) {
		var playerDisplayName = function (player) {
			return (player.bA === '') ? ('Player ' + $elm$core$String$fromInt(player.aG)) : player.bA;
		};
		var maybePlayerEntries = $author$project$Utils$listUnwrapMaybe(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var pid = _v0.a;
					var maybePInfo = _v0.b;
					var maybeLog = _v0.c;
					return A2(
						$elm$core$Maybe$andThen,
						function (pInfo) {
							return A2(
								$elm$core$Maybe$map,
								function (log) {
									return {
										E: pid,
										bA: pInfo.az,
										ac: playerDisplayName(pInfo) + '\'s commander',
										bW: $elm$core$String$fromInt(
											$author$project$Log$current(log))
									};
								},
								maybeLog);
						},
						maybePInfo);
				},
				A2(
					$elm$core$List$map,
					function (pid) {
						return _Utils_Tuple3(
							pid,
							A2($elm$core$Dict$get, pid, model.c),
							A2($elm$core$Dict$get, pid, selectedPlayer.D));
					},
					model.o)));
		var commanderCounter = function (playerList) {
			return A2(
				$author$project$Counter$viewMultiCounter,
				$author$project$Main$themeCommander,
				{
					bk: playerList,
					cL: selectedPlayer.al,
					E: model.j,
					_: 'Damage to ' + selectedPlayer.bA,
					a$: function (id) {
						return A2($author$project$Main$UpdateCommanderDamage, id, selectedPlayer.al);
					},
					bE: $author$project$Main$EditCommanderName,
					bF: $author$project$Main$SelectCommander(selectedPlayer.aG)
				});
		};
		return A2(
			$elm$core$Maybe$withDefault,
			$mdgriffith$elm_ui$Element$text('Error displaying commander counter'),
			A2($elm$core$Maybe$map, commanderCounter, maybePlayerEntries));
	});
var $author$project$Main$themeHealth = {
	co: A3($mdgriffith$elm_ui$Element$rgb, 0.9, 0.9, 0.9),
	bd: A3($mdgriffith$elm_ui$Element$rgb, 0.9, 0.3, 0.3),
	aw: A3($mdgriffith$elm_ui$Element$rgb, 1.0, 0.3, 0.3)
};
var $mdgriffith$elm_ui$Internal$Model$Top = 0;
var $mdgriffith$elm_ui$Element$alignTop = $mdgriffith$elm_ui$Internal$Model$AlignY(0);
var $author$project$Counter$viewBasicCounter = F2(
	function (theme, data) {
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
				]),
			_List_fromArray(
				[
					A3($author$project$Counter$viewCounterLeftButtons, theme, data.E, data.ab),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$width(
							$mdgriffith$elm_ui$Element$fillPortion(2)),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$Border$color(theme.aw),
							$mdgriffith$elm_ui$Element$Border$widthEach(
							{bc: 0, bv: 8, bT: 8, b$: 0})
						]),
					A2(
						$mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width(
								$mdgriffith$elm_ui$Element$fillPortion(2)),
								$mdgriffith$elm_ui$Element$alignTop,
								A2($mdgriffith$elm_ui$Element$paddingXY, 60, 60)
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$Font$size(48)
									]),
								A2(
									$mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$centerY]),
									$mdgriffith$elm_ui$Element$text(data._))),
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$Font$size(48),
										$mdgriffith$elm_ui$Element$alignRight
									]),
								A2(
									$mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$centerY]),
									$mdgriffith$elm_ui$Element$text(data.bW)))
							]))),
					A3($author$project$Counter$viewCounterRightButtons, theme, data.E, data.ab)
				]));
	});
var $author$project$Main$viewHealthCounter = function (selectedPlayer) {
	return A2(
		$author$project$Counter$viewBasicCounter,
		$author$project$Main$themeHealth,
		{
			E: selectedPlayer.aG,
			_: selectedPlayer.bA,
			ab: $author$project$Main$UpdateHealth,
			bW: $elm$core$String$fromInt(
				$author$project$Log$current(selectedPlayer.P))
		});
};
var $author$project$Main$themePoison = {
	co: A3($mdgriffith$elm_ui$Element$rgb, 0.9, 0.9, 0.9),
	bd: A3($mdgriffith$elm_ui$Element$rgb, 0.3, 0.8, 0.3),
	aw: A3($mdgriffith$elm_ui$Element$rgb, 0.3, 0.7, 0.3)
};
var $author$project$Main$viewPoisonCounter = function (selectedPlayer) {
	return A2(
		$author$project$Counter$viewBasicCounter,
		$author$project$Main$themePoison,
		{
			E: selectedPlayer.aG,
			_: selectedPlayer.bA,
			ab: $author$project$Main$UpdatePoison,
			bW: $elm$core$String$fromInt(
				$author$project$Log$current(selectedPlayer.U))
		});
};
var $author$project$Main$viewCountPanel = F2(
	function (model, selectedPlayer) {
		var _v0 = model.C;
		switch (_v0) {
			case 1:
				return $author$project$Main$viewPoisonCounter(selectedPlayer);
			case 2:
				return A2($author$project$Main$viewCommanderCounter, model, selectedPlayer);
			default:
				return $author$project$Main$viewHealthCounter(selectedPlayer);
		}
	});
var $author$project$Main$AddPlayer = function (a) {
	return {$: 7, a: a};
};
var $author$project$Main$EditPlayerName = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Main$SelectPlayer = function (a) {
	return {$: 8, a: a};
};
var $author$project$Main$namesPanelTheme = function (accent) {
	return {
		aS: accent.as,
		t: 4,
		aE: 48,
		bB: A3($mdgriffith$elm_ui$Element$rgb, 0, 0, 0),
		bX: A3($mdgriffith$elm_ui$Element$rgb, 0, 0, 0)
	};
};
var $author$project$Badges$Create = function (a) {
	return {$: 1, a: a};
};
var $author$project$Badges$Existing = function (a) {
	return {$: 0, a: a};
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm_community$list_extra$List$Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var xs_ = A2($elm$core$List$drop, step, xs);
		var okayXs = $elm$core$List$length(xs) > 0;
		var okayArgs = (size > 0) && (step > 0);
		return (okayArgs && okayXs) ? A2(
			$elm$core$List$cons,
			A2($elm$core$List$take, size, xs),
			A3($elm_community$list_extra$List$Extra$greedyGroupsOfWithStep, size, step, xs_)) : _List_Nil;
	});
var $elm_community$list_extra$List$Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3($elm_community$list_extra$List$Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var $author$project$Badges$viewExistingBadge = F2(
	function (theme, badge) {
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Border$color(theme.aS),
					$mdgriffith$elm_ui$Element$Border$widthEach(
					{bc: 2 * theme.t, bv: theme.t, bT: theme.t, b$: theme.t}),
					A2($mdgriffith$elm_ui$Element$paddingXY, 25, 25),
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
				]),
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$Input$text,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Border$color(
							A4($mdgriffith$elm_ui$Element$rgba, 1, 1, 1, 0)),
							$mdgriffith$elm_ui$Element$Events$onFocus(badge.bF),
							$mdgriffith$elm_ui$Element$Font$size(theme.aE),
							$mdgriffith$elm_ui$Element$Font$color(theme.bB),
							$mdgriffith$elm_ui$Element$htmlAttribute(
							$elm$html$Html$Attributes$id(badge.aG))
						]),
					{
						_: $mdgriffith$elm_ui$Element$Input$labelHidden(badge._),
						ab: badge.ab,
						ac: A2(
							$elm$core$Maybe$map,
							function (t) {
								return A2(
									$mdgriffith$elm_ui$Element$Input$placeholder,
									_List_Nil,
									$mdgriffith$elm_ui$Element$text(t));
							},
							badge.ac),
						b_: badge.bA
					}),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$size(theme.aE),
							$mdgriffith$elm_ui$Element$Font$color(theme.bX),
							A2($mdgriffith$elm_ui$Element$paddingXY, 10, 10)
						]),
					$mdgriffith$elm_ui$Element$text(badge.bW))
				]));
	});
var $author$project$Badges$viewNewBadge = F2(
	function (theme, badge) {
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Border$color(theme.aS),
					$mdgriffith$elm_ui$Element$Border$widthEach(
					{bc: 2 * theme.t, bv: theme.t, bT: theme.t, b$: theme.t}),
					A2($mdgriffith$elm_ui$Element$paddingXY, 25, 25),
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
				]),
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$Input$text,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Border$color(
							A4($mdgriffith$elm_ui$Element$rgba, 1, 1, 1, 0)),
							$mdgriffith$elm_ui$Element$Font$size(theme.aE),
							$mdgriffith$elm_ui$Element$htmlAttribute(
							$elm$html$Html$Attributes$id(badge.aG))
						]),
					{
						_: $mdgriffith$elm_ui$Element$Input$labelHidden(badge._),
						ab: badge.ab,
						ac: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_ui$Element$Input$placeholder,
								_List_Nil,
								$mdgriffith$elm_ui$Element$text(badge.ac))),
						b_: ''
					})
				]));
	});
var $author$project$Badges$viewBadge = F2(
	function (theme, badge) {
		if (!badge.$) {
			var existing = badge.a;
			return A2($author$project$Badges$viewExistingBadge, theme, existing);
		} else {
			var _new = badge.a;
			return A2($author$project$Badges$viewNewBadge, theme, _new);
		}
	});
var $author$project$Badges$viewBadgeRow = F2(
	function (theme, badges) {
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
				]),
			A2(
				$elm$core$List$map,
				$author$project$Badges$viewBadge(theme),
				badges));
	});
var $author$project$Badges$viewBadgeColumns = F4(
	function (theme, columns, existingBadges, newBadge) {
		var allBadges = _Utils_ap(
			A2($elm$core$List$map, $author$project$Badges$Existing, existingBadges),
			_List_fromArray(
				[
					$author$project$Badges$Create(newBadge)
				]));
		var badgeGroups = A2($elm_community$list_extra$List$Extra$greedyGroupsOf, columns, allBadges);
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
				]),
			A2(
				$elm$core$List$map,
				$author$project$Badges$viewBadgeRow(theme),
				badgeGroups));
	});
var $author$project$Main$viewNameBadges = F2(
	function (accent, model) {
		var theme = $author$project$Main$namesPanelTheme(accent);
		var playerStat = function (player) {
			var _v0 = model.C;
			if (_v0 === 1) {
				return $elm$core$String$fromInt(
					$author$project$Log$current(player.U));
			} else {
				return $elm$core$String$fromInt(
					$author$project$Log$current(player.P));
			}
		};
		var playerMaybes = A2(
			$elm$core$List$map,
			function (id) {
				return A2($elm$core$Dict$get, id, model.c);
			},
			model.o);
		var playerBadge = function (player) {
			return {
				aG: 'player-' + $elm$core$String$fromInt(player.aG),
				_: 'Edit player ' + $elm$core$String$fromInt(player.aG),
				bA: player.bA,
				ab: $author$project$Main$EditPlayerName(player.aG),
				bF: $author$project$Main$SelectPlayer(player.aG),
				ac: $elm$core$Maybe$Just(
					'Player ' + $elm$core$String$fromInt(player.aG)),
				bW: playerStat(player)
			};
		};
		var newBadge = {
			aG: 'player-' + $elm$core$String$fromInt(model.G),
			_: 'New player name',
			ab: $author$project$Main$AddPlayer,
			ac: 'New Player'
		};
		var maybePlayers = $author$project$Utils$listUnwrapMaybe(playerMaybes);
		var maybeBadges = A2(
			$elm$core$Maybe$map,
			function (players) {
				return A2($elm$core$List$map, playerBadge, players);
			},
			maybePlayers);
		var columns = ($elm$core$List$length(model.o) < 4) ? 1 : (($elm$core$List$length(model.o) < 6) ? 2 : 3);
		return A2(
			$elm$core$Maybe$withDefault,
			$mdgriffith$elm_ui$Element$text('Error displaying player badges'),
			A2(
				$elm$core$Maybe$map,
				function (badges) {
					return A4($author$project$Badges$viewBadgeColumns, theme, columns, badges, newBadge);
				},
				maybeBadges));
	});
var $author$project$Main$viewAccented = F2(
	function (accent, model) {
		return A2(
			$mdgriffith$elm_ui$Element$layout,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Border$solid,
					$mdgriffith$elm_ui$Element$Border$width(8),
					$mdgriffith$elm_ui$Element$Border$color(accent.as),
					$mdgriffith$elm_ui$Element$Border$rounded(8)
				]),
			A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
					]),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Element$height(
								$mdgriffith$elm_ui$Element$fillPortion(1))
							]),
						A2($author$project$Main$viewNameBadges, accent, model)),
						A2(
						$mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
							]),
						_List_fromArray(
							[
								A2($author$project$Main$viewActiveLog, accent, model)
							])),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Element$height(
								$mdgriffith$elm_ui$Element$fillPortion(2))
							]),
						A2(
							$elm$core$Maybe$withDefault,
							$mdgriffith$elm_ui$Element$text('Error finding player'),
							A2(
								$elm$core$Maybe$map,
								$author$project$Main$viewCountPanel(model),
								A2(
									$elm$core$Maybe$andThen,
									function (id) {
										return A2($elm$core$Dict$get, id, model.c);
									},
									$elm$core$List$head(
										A2($elm$core$List$drop, model.j, model.o)))))),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
							]),
						$author$project$Main$viewCountModes(accent))
					])));
	});
var $author$project$Main$view = function (model) {
	var _v0 = model.C;
	switch (_v0) {
		case 0:
			return A2($author$project$Main$viewAccented, $author$project$Main$accentHealth, model);
		case 1:
			return A2($author$project$Main$viewAccented, $author$project$Main$accentPoison, model);
		case 2:
			return A2($author$project$Main$viewAccented, $author$project$Main$accentCommander, model);
		case 3:
			return A2($author$project$Main$viewAccented, $author$project$Main$accentMana, model);
		default:
			return A2($author$project$Main$viewAccented, $author$project$Main$accentCustom, model);
	}
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{cV: $author$project$Main$init, dv: $author$project$Main$subscriptions, dM: $author$project$Main$update, dN: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));