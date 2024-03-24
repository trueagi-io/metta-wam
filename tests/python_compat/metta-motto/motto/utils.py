def get_string_value(atom) -> str:
    item = repr(atom)
    if len(item) > 2 and item[0] == '"' and item[-1] == '"':
        item = item[1:-1]
    return item


def contains_str(value, substring) -> bool:
    str1 = get_string_value(value)
    substring = get_string_value(substring)
    return substring.lower() in str1.lower()


def concat_str(left, right) -> str:
    str1 = get_string_value(left)
    str2 = get_string_value(right)
    return str1 + str2
