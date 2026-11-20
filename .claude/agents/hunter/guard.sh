#!/usr/bin/env bash
# The hunter's shell: the PreToolUse hook of `.claude/agents/hunter.md`, admitting the readers a hunt needs and the probes a hunter puts to a snapshot's compiler and board, by their paths as `snapshot.sh` beside it judges them, and refusing every other form: no build, no test, no directory entered. It judges the words the shell would hand each program rather than the text, so a quoted flag is seen and a quoted `>` is no redirection; anything it cannot judge it refuses.
set -euo pipefail
verdict=''
trap 'case $verdict in admitted) exit 0 ;; refused) exit 2 ;; *) echo "hunter guard: could not judge the command, so it is refused" >&2; exit 2 ;; esac' EXIT
source "$(dirname "${BASH_SOURCE[0]}")/snapshot.sh"

refuse() {
    printf 'hunter guard: refused `%s`: %s. What a hunter may run is listed in .claude/agents/hunter/guard.sh.\n' "$1" "$2" >&2
    verdict=refused
    exit 2
}

# What the compiler is asked: a run or a wonder, never a build or a test, whose outputs are files.
judge_curios() {
    case ${1-} in
        run | wonder) ;;
        *) refuse "curios $*" 'the compiler is asked to run or to wonder, nothing else' ;;
    esac
}

judge_xboard() {
    case ${1-} in
        probe | report) ;;
        *) refuse "xboard $*" 'the board is probed or reported, nothing else' ;;
    esac
}

# Readings of the repository. A leading option is refused since `-c` and `-C` reach configuration, where an alias runs a shell; `--output` is the file every reading can write, and `-O` opens an editor.
judge_git() {
    local subcommand=${1-}
    if [[ $subcommand == --no-pager ]]; then
        shift
        subcommand=${1-}
    fi
    case $subcommand in
        log | show | blame | diff | status | grep | rev-parse | rev-list | ls-files | shortlog | describe) ;;
        *) refuse "git $*" 'not a reading of the repository' ;;
    esac
    local argument
    for argument in "$@"; do
        if [[ $argument == --output* || $argument == -O* || $argument == --open-files-in-pager* ]]; then
            refuse "git $*" 'an output file, an editor or a pager'
        fi
    done
}

# One simple command, by the words the shell would hand it.
judge() {
    local program=$1
    shift
    case $program in
        timeout)
            if [[ ! ${1-} =~ ^[0-9]+[smh]?$ ]]; then
                refuse "timeout $*" 'a timeout here is a duration and the command it bounds'
            fi
            shift
            judge "$@"
            ;;
        time) judge "$@" ;;
        git) judge_git "$@" ;;
        rg)
            local argument
            for argument in "$@"; do
                if [[ $argument == --pre* || $argument == --hostname-bin* ]]; then
                    refuse "rg $*" 'rg --pre and --hostname-bin run a program'
                fi
            done
            ;;
        sed)
            # The one sed a reader needs, a line range printed: anything else sed takes can write a file.
            if [[ $# -ne 3 || $1 != -n || ! $2 =~ ^[0-9]+(,[0-9]+)?p$ || $3 == -* ]]; then
                refuse "sed $*" 'sed here prints a line range of one file, `sed -n N,Mp FILE`, and nothing else'
            fi
            ;;
        sort)
            local argument
            for argument in "$@"; do
                if [[ $argument == --output* || $argument == -*o* ]]; then
                    refuse "sort $*" 'sort -o writes a file'
                fi
            done
            ;;
        uniq)
            local positional=0 argument
            for argument in "$@"; do
                if [[ $argument != -* ]]; then
                    positional=$((positional + 1))
                fi
            done
            if ((positional > 1)); then
                refuse "uniq $*" 'a second file names where uniq writes'
            fi
            ;;
        cat | head | tail | wc | ls | grep | diff | cut | tr | echo | printf | pwd | true) ;;
        /*)
            local binary
            if ! binary=$(snapshot_binary "$program"); then
                refuse "$program $*" "a program named by its path is a snapshot's compiler or board, \`.claude/snapshots/<commit>/binaries/curios\` or \`binaries/xboard\`"
            fi
            case $binary in
                curios) judge_curios "$@" ;;
                xboard) judge_xboard "$@" ;;
            esac
            ;;
        *) refuse "$program $*" 'not a probe or a reader a hunt admits' ;;
    esac
}

command=$(jq -r '.tool_input.command // empty')
if [[ -z $command ]]; then
    verdict=admitted
    exit 0
fi

# A heredoc is opened by the scan below, where it meets `<<` outside quotes and comments, so it and the shell agree on which lines are a body. The one heredoc a probe needs is `<<'NAME'`, one name in quotes: its body is the program on standard input, data the shell neither expands nor joins, dropped unread up to the line that is the name. Every other `<<` is refused: a bare delimiter's body is the shell's to expand, and a here-string's text is.
text=$command
pending=()

# The scan: quotes are resolved into words, a pipe, a list or a subshell ends a command, and what the shell would evaluate itself — a substitution, a parameter, a brace, a background `&`, a redirection past a descriptor merge or /dev/null — is refused rather than guessed at.
words=()
word=''
in_word=0

end_word() {
    if ((in_word)); then
        words+=("$word")
    fi
    word=''
    in_word=0
}

end_command() {
    end_word
    if ((${#words[@]})); then
        judge "${words[@]}"
    fi
    words=()
}

length=${#text}
i=0
while ((i < length)); do
    c=${text:i:1}
    case $c in
        "'")
            rest=${text:i+1}
            if [[ $rest != *"'"* ]]; then
                refuse "$text" 'an unclosed quote'
            fi
            quoted=${rest%%\'*}
            word+=$quoted
            in_word=1
            i=$((i + ${#quoted} + 2))
            continue
            ;;
        '"')
            i=$((i + 1))
            while :; do
                if ((i >= length)); then
                    refuse "$text" 'an unclosed quote'
                fi
                d=${text:i:1}
                case $d in
                    '"') break ;;
                    '\')
                        # Inside double quotes a backslash escapes only what would end or expand them, joins a line, and is itself a character before anything else.
                        case ${text:i+1:1} in
                            $'\n') ;;
                            '$' | '`' | '"' | '\') word+=${text:i+1:1} ;;
                            *) word+="\\${text:i+1:1}" ;;
                        esac
                        i=$((i + 2))
                        continue
                        ;;
                    '`' | '$') refuse "$text" 'a substitution or a parameter inside quotes' ;;
                    *) word+=$d ;;
                esac
                i=$((i + 1))
            done
            in_word=1
            i=$((i + 1))
            continue
            ;;
        '\')
            if ((i + 1 >= length)); then
                refuse "$text" 'a backslash that escapes nothing'
            fi
            # Before a newline it joins two lines, as the shell reads them, and is no character of either.
            if [[ ${text:i+1:1} != $'\n' ]]; then
                word+=${text:i+1:1}
                in_word=1
            fi
            i=$((i + 2))
            continue
            ;;
        '#')
            if ((in_word)); then
                word+=$c
            else
                rest=${text:i}
                rest=${rest%%$'\n'*}
                i=$((i + ${#rest}))
                continue
            fi
            ;;
        ' ' | $'\t') end_word ;;
        $'\n')
            end_command
            i=$((i + 1))
            # Each heredoc opened on the line just ended takes the lines up to its name, in the order they were opened.
            for name in "${pending[@]}"; do
                closed=0
                while ((i < length)); do
                    rest=${text:i}
                    line=${rest%%$'\n'*}
                    i=$((i + ${#line} + 1))
                    if [[ $line == "$name" ]]; then
                        closed=1
                        break
                    fi
                done
                if ((!closed)); then
                    refuse "$text" 'an unclosed heredoc'
                fi
            done
            pending=()
            continue
            ;;
        ';' | '|') end_command ;;
        '(' | ')') end_command ;;
        # A brace groups commands only where a command may start and is a word anywhere else, and inside a word the shell expands it into several: neither is read here.
        '{' | '}') refuse "$text" 'a brace the shell would group by or expand' ;;
        '<')
            if [[ ${text:i+1:1} == '<' ]]; then
                rest=${text:i+2}
                if [[ ! $rest =~ ^[[:blank:]]*(\'([A-Za-z_][A-Za-z0-9_]*)\'|\"([A-Za-z_][A-Za-z0-9_]*)\")([[:blank:];|&\)<>]|$'\n'|$) ]]; then
                    refuse "$text" "a heredoc opens with one name in quotes, \`<<'NAME'\`, so its body is data"
                fi
                pending+=("${BASH_REMATCH[2]}${BASH_REMATCH[3]}")
                end_word
                opener=${BASH_REMATCH[0]}
                i=$((i + 2 + ${#opener} - ${#BASH_REMATCH[4]}))
                continue
            fi
            ;;
        '`' | '$') refuse "$text" 'a substitution or a parameter the shell would evaluate' ;;
        '&')
            if [[ ${text:i+1:1} != '&' ]]; then
                refuse "$text" 'a detached command'
            fi
            end_command
            i=$((i + 2))
            continue
            ;;
        '>')
            # Two redirections write no file, a merge into a descriptor and `/dev/null`, each only where the word ends after it: `>&1x` is a file named `1x`.
            rest=${text:i+1}
            if [[ $word =~ ^[0-9]*$ && $rest =~ ^(&[0-9]+|[[:blank:]]*/dev/null)([[:blank:];|&\)<>]|$'\n'|$) ]]; then
                word=''
                in_word=0
                i=$((i + 1 + ${#BASH_REMATCH[1]}))
                continue
            fi
            refuse "$text" 'a redirection writes a file'
            ;;
        *)
            word+=$c
            in_word=1
            ;;
    esac
    i=$((i + 1))
done
end_command
if ((${#pending[@]})); then
    refuse "$text" 'an unclosed heredoc'
fi
verdict=admitted
