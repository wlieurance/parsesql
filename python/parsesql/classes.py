import colorama as cr
import re
import os
from parsesql.vars import STANDARDS, SPECIAL


class SqlParser:
    """
    A class to represent SQL statements and their attributes.

    :param text: Text containing one or more SQL statements separated by a semicolon.
    :type text: str
    :param file: A path to a file containing SQL to parse.
    :type file: str
    :param standard: An SQL standard from which to identify key words from. One of: ('SQL-92', 'SQL:2011',
        'SQL:2016', 'PostgreSQL')
    :type standard: str
    :param params: bracketed text in the SQL to replace with a parameter.
    :type params: dict
    """
    def __init__(self, text=None, file=None, standard='SQL:2016', params=None):
        """Constructor method"""
        if standard not in STANDARDS:
            raise ValueError(f"standard: must be one of {STANDARDS}.")
        if not isinstance(params, dict) and params is not None:
            raise ValueError("params: must be dictionary.")
        if not isinstance(text, str) and text:
            raise ValueError("text: must be string.")
        if not text and not file:
            raise ValueError("either text or file must not be None.")
        if text and file:
            raise ValueError("text and file cannot both be not both be provided.")
        self.standard = standard
        """An SQL standard from which to identify key words from. One of: ('SQL-92', 'SQL:2011', 'SQL:2016', 
        'PostgreSQL')
        :type standard: str
        """
        self.reserved = [x[0] for x in SPECIAL if x[1] == standard]
        """Populated from the global 'STANDARDS' variable and filtered during class initialization by choice of the 
        `standard` field.
        :type reserved: List[str]
        """
        if params is None:
            self.params = dict()
            """Contains parameters to replace in the SQL code. Parameters need to be identically named and contained by
            brackets {} in the SQL file and dictionary names must match the text enclosed in brackets.
            :type params: dict"""
        else:
            self.params = params
        if text:
            self.text = text
            """Text containing one or more SQL statements separated by a semicolon.
            :type text: str"""
        else:
            self.read_file(file)
        if self.params:
            self.param_text = self.text.format(**self.params)
            """Sourced from `text` or `file` where parameters which have been enclosed in brackets in the string have 
            been replaced by their values in `params`.
            :type param_text: str"""
        else:
            self.param_text = self.text
        self.char_states = []
        """Each list element is an SQL statement and each sublist is a list of two values in a tuple, `char` and 
        `state`, a single character and a dictionary containing that character's state respectively.
        :type char_states: List[List[(str, dict)]]"""
        self.stripped_states = []
        """Contains the same data as `char_states`, but with whitespace trimmed from the beginning and end of each 
        statement sublist.
        :type stripped_states: List[List[(str, dict)]]"""
        self.string_states = []
        """Contains the same data as `stripped_states`, but the characters sharing the same sequential state have been
        concatenated and assigned their shared state.
        :type string_states: List[List[(str, dict)]]"""
        self.sql = []
        """The final SQL statements, split into list format.
        :type sql: List[str]"""
        self.formatted = []
        """The final SQL statements, split into list format and formatted with color and style via the colorama 
        module.
        :type formatted: List[str]"""
        self.get_state()
        self.strip_ws()
        self.combine_states()
        self.combine_stmts()

    def __str__(self):
        """Overloaded str function for printing the class instance.

        :return: formatted SQL concatenated by a dashed line.
        :rtype: str
        """
        cr.init()
        s = ''.join(('\n\n', '-' * 70, '\n\n')).join(self.formatted)
        return s

    @staticmethod
    def format_char(my_chars, my_state, reserved=None):
        """Takes a character and a state and assigns a color and style to the character using the colorama package.

        :param my_chars: A character string containing the text to format.
        :type my_chars: str
        :param my_state: A variable containing state information for my_chars.
        :type mystate: dict

        :return: A color/style formatted version of my_chars.
        :rtype: str
        """

        if my_state['lc'] or my_state['bc']:
            text = cr.Style.RESET_ALL + cr.Fore.YELLOW + my_chars
        elif any(v for (k, v) in my_state.items() if k in ['dt1', 'dt2']):
            text = cr.Style.RESET_ALL + cr.Back.YELLOW + cr.Fore.RED + my_chars
        elif any(v for (k, v) in my_state.items() if k in ['sq', 'dc']):
            text = cr.Style.RESET_ALL + cr.Fore.RED + my_chars
        elif my_state['qi']:
            text = cr.Style.RESET_ALL + cr.Fore.CYAN + my_chars
        else:
            if reserved:
                split_text = re.split(r'(\s+|,|;)', my_chars)
                format_list = [cr.Fore.BLUE + cr.Style.BRIGHT +
                               x if x.upper() in reserved else cr.Style.RESET_ALL + x
                               for x in split_text]
                text = cr.Style.RESET_ALL + ''.join(format_list)
            else:
                text = cr.Style.RESET_ALL + my_chars
        return text

    def read_file(self, file):
        """Assigns the `text` instance variable with the contents of `file`.

        :param file: The path to a file containing SQL to parse.
        :type file: str
        """
        if not os.path.isfile(file):
            raise ValueError("file: must be an existing file.")
        with open(file, 'r') as f:
            self.text = f.read()

    def get_state(self):
        """Parses character string multi-statement SQL one character at a time and assigns a state to that character
        pertaining to whether it is single-quoted, dollar quoted, a quoted identifier, or a comment.
        """

        # replaces items in the sql designated as parameters by an enclosing {}
        sql_chars = list(self.param_text)
        char_len = len(sql_chars)
        state_list = []
        default_state = {
            'lc': False,  # line comment
            'bc': False,  # block comment
            'dt1': False,  # first tag within the dollar quote
            'dt2': False,  # second tag within the dollar quote
            'dc': False,  # string constant within the dollar quote
            'sq': False,  # single quoted string constant
            'qi': False  # quoted identifier
        }
        state = default_state.copy()
        begin = True
        for i in range(char_len):
            new_state = state.copy()

            if ''.join(sql_chars[i:min(i+2, char_len)]) == '--':
                if all(not value for value in new_state.values()):  # checks that all state values are False
                    new_state['lc'] = True
                    begin = True

            if sql_chars[i] == '\n':
                if new_state['lc']:
                    new_state['lc'] = False
                    begin = False

            if ''.join(sql_chars[i:min(i+2, char_len)]) == '/*':
                if all(not value for value in new_state.values()):
                    new_state['bc'] = True
                    begin = True

            if ''.join(sql_chars[max(i-1, 0):i+1]) == '*/':
                if new_state['bc']:
                    new_state['bc'] = False
                    begin = False

            if sql_chars[i] == '$':
                if all(not value for value in new_state.values()):
                    new_state['dt1'] = True
                    begin = True
                # this next control statement checks that a certain state is True and all other states are False
                elif new_state['dt1'] and all(not v for (k, v) in new_state.items() if k != 'dt1'):
                    new_state['dt1'] = False
                    new_state['dc'] = True
                    begin = False
                elif new_state['dc'] and all(not v for (k, v) in new_state.items() if k != 'dc'):
                    begin = True
                    new_state['dc'] = False
                    new_state['dt2'] = True
                elif new_state['dt2'] and all(not v for (k, v) in new_state.items() if k != 'dt2'):
                    new_state['dt2'] = False
                    begin = False

            if (sql_chars[i] == "'" and
                    ''.join(sql_chars[i:min(i+2, char_len)]) != "''" and
                    ''.join(sql_chars[max(i-1, 0):i+1]) != "''"):
                if all(not value for value in new_state.values()):
                    new_state['sq'] = True
                    begin = True
                elif new_state['sq'] and all(not v for (k, v) in new_state.items() if k != 'sq'):
                    new_state["sq"] = False
                    begin = False

            if (sql_chars[i] == '"' and
                    ''.join(sql_chars[i:min(i+2, char_len)]) != '""' and
                    ''.join(sql_chars[max(i-1, 0):i+1]) != '""'):
                if all(not value for value in new_state.values()):
                    new_state['qi'] = True
                    begin = True
                elif new_state['qi'] and all(not v for (k, v) in new_state.items() if k != 'qi'):
                    new_state["qi"] = False
                    begin = False

            if begin:
                state_list.append((sql_chars[i], new_state))
            else:
                state_list.append((sql_chars[i], state))
            state = new_state.copy()

            if sql_chars[i] == ";" and all(not value for value in new_state.values()):
                self.char_states.append(state_list)
                state_list = []
        if state_list:
            self.char_states.append(state_list)

    def strip_ws(self):
        """Finds white space characters at the beginning and end of a list produced by the `get_state` method and
        removes them."""
        for stmt_list in self.char_states:
            new_list = stmt_list.copy()
            for i in range(2):
                new = []
                non_ws = False
                for char, state in new_list:
                    if not str.isspace(char):
                        non_ws = True
                        new.append((char, state))
                    elif str.isspace(char) and non_ws:
                        new.append((char, state))
                new.reverse()
                new_list = new.copy()
            self.stripped_states.append(new_list)

    def combine_states(self):
        """Combines characters that have the same state into strings and stores them along with state and order in a
        list."""
        prev_state = None
        for stmt_chars in self.stripped_states:
            concat = []
            raw = str()
            for char, state in stmt_chars:
                if state == prev_state or prev_state is None:
                    raw += char
                else:
                    concat.append((raw, prev_state))
                    raw = char
                prev_state = state
            if raw:
                concat.append((raw, prev_state))
            self.string_states.append(concat)

    def combine_stmts(self):
        """Combines SQL with different states into a single statements, both unformatted for execution and formatted
        for printing/display.
        """
        for stmt_chars in self.string_states:
            raw = str()
            formatted = str()
            for char, state in stmt_chars:
                fchar = self.format_char(char, state, self.reserved)
                raw += char
                formatted += fchar
                # print(stmt[i], fchar)
            if raw.strip():
                self.sql.append(raw)
                self.formatted.append(formatted)
