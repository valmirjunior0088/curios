#!/usr/bin/env bash
# The hunter's shell: the PreToolUse hook of `.claude/agents/hunter.md`, admitting the readers a hunt needs and the probes, builds, tests and reverts a hunter makes in its worktree, as `worktree.sh` beside it judges one, and refusing every other form. It judges the words bash would hand each program rather than the text, so a quoted flag is seen and a quoted `>` is no redirection; anything it cannot judge it refuses.
set -euo pipefail
trap 'echo "hunter guard: could not judge the command, so it is refused" >&2; exit 2' ERR
source "$(dirname "${BASH_SOURCE[0]}")/worktree.sh"

refuse() {
    printf 'hunter guard: refused `%s`: %s. What a hunter may run is listed in .claude/agents/hunter/guard.sh.\n' "$1" "$2" >&2
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

# The directory a subshell entered, empty outside one: `cd` is judged only as the first step of `(cd <worktree> && …)`, so no directory outlives the command it was entered for.
entered=''
entered_stack=()

judge_cd() {
    local within
    if ((${#entered_stack[@]} == 0)); then
        refuse "cd $*" 'a directory is entered only inside a subshell, `(cd <worktree> && …)`, so it does not outlive the command'
    fi
    if [[ $# -ne 1 || $1 != /* || ! -d $1 ]] || ! within=$(within_worktree "$1") || [[ -n $within ]]; then
        refuse "cd $*" "cd enters the root of a hunter's worktree, by its absolute path, and nothing else"
    fi
    entered=$(realpath -m -- "$1")
}

# Cargo in a hunter's worktree alone, entered by `(cd <worktree> && cargo …)`, so nothing a hunter builds or runs is the main checkout's: that checkout moves under its own commits while a hunt runs. A build, a test, or the compiler and the board run there, with the words that follow judged as the program's own. An option that reaches configuration, the compiler's flags or another target directory is refused: each can run a program, or write outside the worktree.
judge_cargo() {
    if [[ -z $entered ]]; then
        refuse "cargo $*" "cargo runs in a hunter's worktree, entered by a subshell: (cd <worktree> && cargo …)"
    fi
    local subcommand=${1-}
    shift || true
    local argument
    for argument in "$@"; do
        case $argument in
            --config* | -Z* | --target-dir* | --manifest-path* | -C*) refuse "cargo $subcommand $*" 'an option that reaches configuration or writes outside the worktree' ;;
        esac
    done
    case $subcommand in
        build | test) ;;
        xboard) judge_xboard "$@" ;;
        xtask)
            if [[ $# -ne 2 || ${1-} != launcher || ${2-} != build ]]; then
                refuse "cargo xtask $*" 'the one xtask a worktree takes is the launcher the compiler embeds, `cargo xtask launcher build`'
            fi
            ;;
        run)
            if [[ ${1-} == --release ]]; then
                shift
            fi
            if [[ ${1-} == --package && ${2-} == curios && ${3-} == -- ]]; then
                judge_curios "${@:4}"
            elif [[ ${1-} == --package && ${2-} == xboard && ${3-} == -- ]]; then
                judge_xboard "${@:4}"
            else
                refuse "cargo run $*" 'cargo run in a worktree runs the compiler or the board, nothing else'
            fi
            ;;
        *) refuse "cargo $subcommand $*" 'cargo in a worktree builds, tests, or runs the compiler or the board' ;;
    esac
}

# Readings of the repository, and a revert of a hunter's worktree inside the subshell that entered it. A leading option is refused since `-c` and `-C` reach configuration, where an alias runs a shell; `--output` is the file every reading can write, `-O` opens an editor, and `clean -x` would delete the worktree's build with the rest.
judge_git() {
    local subcommand=${1-}
    if [[ $subcommand == --no-pager ]]; then
        shift
        subcommand=${1-}
    fi
    case $subcommand in
        log | show | blame | diff | status | grep | rev-parse | rev-list | ls-files | shortlog | describe) ;;
        restore | clean)
            if [[ -z $entered ]]; then
                refuse "git $*" 'only a worktree is reverted, entered by a subshell: (cd <worktree> && git restore …)'
            fi
            ;;
        *) refuse "git $*" 'not a reading of the repository, or a revert of a worktree' ;;
    esac
    local argument
    for argument in "$@"; do
        if [[ $argument == --output* || $argument == -O* ]]; then
            refuse "git $*" 'an output file or an editor'
        fi
        if [[ $subcommand == clean && $argument =~ ^-[^-]*[xX] ]]; then
            refuse "git $*" "git clean -x deletes the worktree's build with the rest"
        fi
    done
}

# One simple command, by the words bash would hand it.
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
        cd) judge_cd "$@" ;;
        cargo) judge_cargo "$@" ;;
        git) judge_git "$@" ;;
        rg)
            local argument
            for argument in "$@"; do
                if [[ $argument == --pre* ]]; then
                    refuse "rg $*" 'rg --pre runs a program'
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
        *) refuse "$program $*" 'not a probe or a reader a hunt admits' ;;
    esac
}

command=$(jq -r '.tool_input.command // empty')
if [[ -z $command ]]; then
    exit 0
fi

# A heredoc body is the program on standard input, data to the shell and never a command: it is dropped up to its delimiter before the scan, so a `=>` arm is no redirection. A line opening several heredocs is followed by their bodies in order, as bash reads them.
text=''
delimiters=()
heredoc='<<-?[[:space:]]*['\''"]?([A-Za-z_][A-Za-z0-9_]*)'
while IFS= read -r line || [[ -n $line ]]; do
    if ((${#delimiters[@]})); then
        trimmed="${line#"${line%%[![:space:]]*}"}"
        if [[ $trimmed == "${delimiters[0]}" ]]; then
            delimiters=("${delimiters[@]:1}")
        fi
        continue
    fi
    text+="$line"$'\n'
    rest=$line
    while [[ $rest =~ $heredoc ]]; do
        delimiters+=("${BASH_REMATCH[1]}")
        rest=${rest#*"${BASH_REMATCH[0]}"}
    done
done <<<"$command"

# The scan: quotes are resolved into words, a pipe, a list, a subshell or a brace group ends a command, a subshell's `cd` lasts until it closes, and what the shell would evaluate itself — a substitution, a parameter, a background `&`, a redirection past a descriptor merge or /dev/null — is refused rather than guessed at.
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
                        word+=${text:i+1:1}
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
            word+=${text:i+1:1}
            in_word=1
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
        $'\n' | ';' | '|' | '{' | '}') end_command ;;
        '(')
            end_command
            entered_stack+=("$entered")
            ;;
        ')')
            end_command
            if ((${#entered_stack[@]} == 0)); then
                refuse "$text" 'a subshell closed that was never opened'
            fi
            entered=${entered_stack[-1]}
            unset 'entered_stack[-1]'
            ;;
        '<') ;;
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
            if [[ ${text:i+1:1} == '&' && ${text:i+2:1} =~ [0-9] && $word =~ ^[0-9]*$ ]]; then
                word=''
                in_word=0
                i=$((i + 3))
                continue
            fi
            rest=${text:i+1}
            rest="${rest#"${rest%%[! ]*}"}"
            if [[ $rest == /dev/null* && $word =~ ^[0-9]*$ ]]; then
                word=''
                in_word=0
                i=$((length - ${#rest} + 9))
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
