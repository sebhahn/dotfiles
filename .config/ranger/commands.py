import re
import os
import os.path
import subprocess
from collections import deque

from ranger.container.file import File
from ranger.core.loader import CommandLoader
from ranger.api.commands import *
from ranger.ext.get_executables import get_executables

class extracthere(Command):

    def execute(self):
        """ Extract copied files to current directory """
        copied_files = tuple(self.fm.copy_buffer)

        if not copied_files:
            return

        def refresh(_):
            cwd = self.fm.get_directory(original_path)
            cwd.load_content()

        one_file = copied_files[0]
        cwd = self.fm.thisdir
        original_path = cwd.path
        au_flags = ['-X', cwd.path]
        au_flags += self.line.split()[1:]
        au_flags += ['-e']

        self.fm.copy_buffer.clear()
        self.fm.cut_buffer = False
        if len(copied_files) == 1:
            descr = "extracting: " + os.path.basename(one_file.path)
        else:
            descr = "extracting files from: " + os.path.basename(
                one_file.dirname)
        obj = CommandLoader(args=['aunpack'] + au_flags \
                + [f.path for f in copied_files], descr=descr)

        obj.signal_bind('after', refresh)
        self.fm.loader.add(obj)


class fasd_dir(Command):
    """
    Pull a list of fasd history, present and narrow the directories with fzf, change directory on selection.
    """

    def execute(self):
        fzf = self.fm.execute_command(
            "fasd -dl | grep -iv cache | fzf 2>/dev/tty",
            universal_newlines=True,
            stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.rstrip('\n'))
            print(fzf_file)
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)


# fzf_fasd - Fasd + Fzf + Ranger (Interactive Style)
class fzf_fasd(Command):
    """
    :fzf_fasd

    Jump to a file or folder using Fasd and fzf

    URL: https://github.com/clvv/fasd
    URL: https://github.com/junegunn/fzf
    """

    def execute(self):
        if self.quantifier:
            command = "fasd | fzf -e -i --tac --no-sort | awk '{ print substr($0, index($0,$2)) }'"
        else:
            command = "fasd | fzf -e -i --tac --no-sort | awk '{ print substr($0, index($0,$2)) }'"
        fzf = self.fm.execute_command(command, stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.decode('utf-8').rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)


# Fasd with ranger (Command Line Style)
# https://github.com/ranger/ranger/wiki/Commands
class cmd_fasd(Command):
    """
    :fasd

    Jump to directory using fasd
    URL: https://github.com/clvv/fasd
    """

    def execute(self):
        arg = self.rest(1)
        if arg:
            directory = subprocess.check_output(
                ["fasd", "-d"] + arg.split(), universal_newlines=True).strip()
            self.fm.cd(directory)


class toggle_flat(Command):
    """
    :toggle_flat

    Flattens or unflattens the directory view.
    """

    def execute(self):
        if self.fm.thisdir.flat == 0:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = -1
            self.fm.thisdir.load_content()
        else:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = 0
            self.fm.thisdir.load_content()


class fasd(Command):
    """
    This command uses fasd to jump to a frequently visited directory with a given substring of its path.
    """

    def execute(self):
        args = self.rest(1).split()
        if args:
            directories = self._get_directories(*args)
            if directories:
                self.fm.cd(directories[0])
            else:
                self.fm.notify("No results from fasd", bad=True)

    def tab(self, tabnum):
        start, current = self.start(1), self.rest(1)
        for path in self._get_directories(*current.split()):
            yield start + path

    @staticmethod
    def _get_directories(*args):
        output = subprocess.check_output(["fasd", "-dl"] + list(args),
                                         universal_newlines=True)
        dirs = output.strip().split("\n")
        dirs.sort(reverse=True)  # Listed in ascending frecency
        return dirs


class fzf_select(Command):
    """
    :fzf_select
    Find a file using fzf.
    With a prefix argument to select only directories.

    See: https://github.com/junegunn/fzf
    """

    def execute(self):

        if 'fzf' not in get_executables():
            self.fm.notify('Could not find fzf in the PATH.', bad=True)
            return

        fd = None
        if 'fdfind' in get_executables():
            fd = 'fdfind'
        elif 'fd' in get_executables():
            fd = 'fd'

        if fd is not None:
            hidden = ('--hidden' if self.fm.settings.show_hidden else '')
            exclude = "--no-ignore-vcs --exclude '.git' --exclude '*.py[co]' --exclude '__pycache__'"
            only_directories = ('--type directory' if self.quantifier else '')
            fzf_default_command = '{} --follow {} {} {} --color=always'.format(
                fd, hidden, exclude, only_directories)
        else:
            hidden = ('-false' if self.fm.settings.show_hidden else
                      r"-path '*/\.*' -prune")
            exclude = r"\( -name '\.git' -o -name '*.py[co]' -o -fstype 'dev' -o -fstype 'proc' \) -prune"
            only_directories = ('-type d' if self.quantifier else '')
            fzf_default_command = 'find -L . -mindepth 1 {} -o {} -o {} -print | cut -b3-'.format(
                hidden, exclude, only_directories)

        env = os.environ.copy()
        env['FZF_DEFAULT_COMMAND'] = fzf_default_command
        env['FZF_DEFAULT_OPTS'] = '--height=40% --layout=reverse --ansi --preview="{}"'.format(
            '''
            (
                batcat --color=always {} ||
                bat --color=always {} ||
                cat {} ||
                tree -ahpCL 3 -I '.git' -I '*.py[co]' -I '__pycache__' {}
            ) 2>/dev/null | head -n 100
        ''')

        fzf = self.fm.execute_command('fzf --no-multi',
                                      env=env,
                                      universal_newlines=True,
                                      stdout=subprocess.PIPE)
        stdout, _ = fzf.communicate()
        if fzf.returncode == 0:
            selected = os.path.abspath(stdout.strip())
            if os.path.isdir(selected):
                self.fm.cd(selected)
            else:
                self.fm.select_file(selected)




class fd_search(Command):
    """
    :fd_search [-d<depth>] <query>
    Executes "fd -d<depth> <query>" in the current directory and focuses the
    first match. <depth> defaults to 1, i.e. only the contents of the current
    directory.

    See https://github.com/sharkdp/fd
    """

    SEARCH_RESULTS = deque()

    def execute(self):

        self.SEARCH_RESULTS.clear()

        if 'fdfind' in get_executables():
            fd = 'fdfind'
        elif 'fd' in get_executables():
            fd = 'fd'
        else:
            self.fm.notify("Couldn't find fd in the PATH.", bad=True)
            return

        if self.arg(1):
            if self.arg(1)[:2] == '-d':
                depth = self.arg(1)
                target = self.rest(2)
            else:
                depth = '-d1'
                target = self.rest(1)
        else:
            self.fm.notify(":fd_search needs a query.", bad=True)
            return

        hidden = ('--hidden' if self.fm.settings.show_hidden else '')
        exclude = "--no-ignore-vcs --exclude '.git' --exclude '*.py[co]' --exclude '__pycache__'"
        command = '{} --follow {} {} {} --print0 {}'.format(
            fd, depth, hidden, exclude, target)
        fd = self.fm.execute_command(command,
                                     universal_newlines=True,
                                     stdout=subprocess.PIPE)
        stdout, _ = fd.communicate()

        if fd.returncode == 0:
            results = filter(None, stdout.split('\0'))
            if not self.fm.settings.show_hidden and self.fm.settings.hidden_filter:
                hidden_filter = re.compile(self.fm.settings.hidden_filter)
                results = filter(
                    lambda res: not hidden_filter.search(os.path.basename(res)
                                                         ), results)
            results = map(
                lambda res: os.path.abspath(
                    os.path.join(self.fm.thisdir.path, res)), results)
            self.SEARCH_RESULTS.extend(sorted(results, key=str.lower))
            if len(self.SEARCH_RESULTS) > 0:
                self.fm.notify('Found {} result{}.'.format(
                    len(self.SEARCH_RESULTS),
                    ('s' if len(self.SEARCH_RESULTS) > 1 else '')))
                self.fm.select_file(self.SEARCH_RESULTS[0])
            else:
                self.fm.notify('No results found.')


class fd_next(Command):
    """
    :fd_next
    Selects the next match from the last :fd_search.
    """

    def execute(self):
        if len(fd_search.SEARCH_RESULTS) > 1:
            fd_search.SEARCH_RESULTS.rotate(-1)  # rotate left
            self.fm.select_file(fd_search.SEARCH_RESULTS[0])
        elif len(fd_search.SEARCH_RESULTS) == 1:
            self.fm.select_file(fd_search.SEARCH_RESULTS[0])


class fd_prev(Command):
    """
    :fd_prev
    Selects the next match from the last :fd_search.
    """

    def execute(self):
        if len(fd_search.SEARCH_RESULTS) > 1:
            fd_search.SEARCH_RESULTS.rotate(1)  # rotate right
            self.fm.select_file(fd_search.SEARCH_RESULTS[0])
        elif len(fd_search.SEARCH_RESULTS) == 1:
            self.fm.select_file(fd_search.SEARCH_RESULTS[0])


class fzf_rga_documents_search(Command):
    """
    :fzf_rga_search_documents
    Search in PDFs, E-Books and Office documents in current directory.
    Allowed extensions: .epub, .odt, .docx, .fb2, .ipynb, .pdf.

    Usage: fzf_rga_search_documents <search string>
    """

    def execute(self):
        if self.arg(1):
            search_string = self.rest(1)
        else:
            self.fm.notify("Usage: fzf_rga_search_documents <search string>",
                           bad=True)
            return

        command = "rga '%s' . --rga-adapters=pandoc,poppler | fzf +m | awk -F':' '{print $1}'" % search_string
        fzf = self.fm.execute_command(command,
                                      universal_newlines=True,
                                      stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.rstrip('\n'))
            self.fm.execute_file(File(fzf_file))


def show_error_in_console(msg, fm):
    fm.notify(msg, bad=True)


def navigate_path(fm, selected):
    if not selected:
        return

    selected = os.path.abspath(selected)
    if os.path.isdir(selected):
        fm.cd(selected)
    elif os.path.isfile(selected):
        fm.select_file(selected)
    else:
        show_error_in_console(f"Neither directory nor file: {selected}", fm)
        return


def select_with_fzf(fzf_cmd, input, fm):
    fm.ui.suspend()
    try:
        # stderr is used to open to attach to /dev/tty
        proc = subprocess.Popen(fzf_cmd,
                                stdout=subprocess.PIPE,
                                stdin=subprocess.PIPE,
                                text=True)
        stdout, _ = proc.communicate(input=input)

        # ESC gives 130
        if proc.returncode not in [0, 130]:
            raise Exception(
                f"Bad process exit code: {proc.returncode}, stdout={stdout}")
    finally:
        fm.ui.initialize()
    return stdout.strip()


class dir_history_navigate(Command):

    def execute(self):
        lst = []
        for d in reversed(self.fm.tabs[self.fm.current_tab].history.history):
            lst.append(d.path)

        fm = self.fm
        selected = select_with_fzf(["fzf"], "\n".join(lst), fm)

        navigate_path(fm, selected)


class fzf_locate(Command):
    """
    :fzf_locate
    Find a file using fzf.
    With a prefix argument select only directories.
    See: https://github.com/junegunn/fzf
    """

    def execute(self):
        if self.quantifier:
            # command = "locate home | fzf -e -i"
            command = "locate pattern | grep -E '^(/home|/data|/data2)' | fzf -e -i"
        else:
            # command = "locate home | fzf -e -i"
            command = "locate pattern | grep -E '^(/home|/data|/data2)' | fzf -e -i"
        fzf = self.fm.execute_command(command, stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.decode('utf-8').rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)
