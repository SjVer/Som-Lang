# somc(1) completion                                        -*- shell-script -*-

_somc_autocomplete()
{
    local cur prev words cword split
    _init_completion -s || return

    case $prev in
        -h|--help|--usage|-V|--version)
            return
            ;;
        -o|--output)
            _filedir
            return
            ;;
    esac

    $split && return

    if [[ $cur == -* ]]; then
        COMPREPLY=( $(compgen -W '$(_parse_help "$1" --help)' -- "$cur") )
        [[ $COMPREPLY == *= ]] && compopt -o nospace
        return
    fi

    _filedir '@(som)'
} &&
complete -F _somc_autocomplete somc
