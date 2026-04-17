#!/usr/bin/env bash
set -o noclobber -o noglob -o nounset -o pipefail
IFS=$'\n'

FILE_PATH="${1}"
PV_WIDTH="${2}"
PV_HEIGHT="${3}"
IMAGE_CACHE_PATH="${4}"
PV_IMAGE_ENABLED="${5}"

FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="$(printf "%s" "${FILE_EXTENSION}" | tr '[:upper:]' '[:lower:]')"
MIMETYPE="$(file --dereference --brief --mime-type -- "${FILE_PATH}")"

handle_image() {
    [[ "${PV_IMAGE_ENABLED}" != 'True' ]] && return

    case "${MIMETYPE}" in
        image/*)
            cp -- "${FILE_PATH}" "${IMAGE_CACHE_PATH}" && exit 6
            exit 1;;
        video/*)
            ffmpegthumbnailer -i "${FILE_PATH}" -o "${IMAGE_CACHE_PATH}" -s 0 2>/dev/null && exit 6
            exit 1;;
        application/pdf)
            pdftoppm -f 1 -l 1 -r 100 -singlefile -jpeg -- "${FILE_PATH}" "${IMAGE_CACHE_PATH%.*}" 2>/dev/null \
                && mv -- "${IMAGE_CACHE_PATH%.*}.jpg" "${IMAGE_CACHE_PATH}" && exit 6
            exit 1;;
    esac
}

handle_extension() {
    case "${FILE_EXTENSION_LOWER}" in
        a|ace|alz|arc|arj|bz|bz2|cab|cpio|deb|gz|jar|lha|lz|lzh|lzma|lzo|\
        rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z|zip)
            atool --list -- "${FILE_PATH}" && exit 5
            bsdtar --list --file "${FILE_PATH}" && exit 5
            exit 1;;
        rar)
            unrar lt -p- -- "${FILE_PATH}" && exit 5; exit 1;;
        7z)
            7z l -- "${FILE_PATH}" && exit 5; exit 1;;
        pdf)
            pdftotext -l 10 -nopgbrk -q -- "${FILE_PATH}" - \
                | fmt -w "${PV_WIDTH}" && exit 5
            exiftool "${FILE_PATH}" && exit 5
            exit 1;;
        md|markdown)
            glow -- "${FILE_PATH}" && exit 5
            bat --color=always -- "${FILE_PATH}" && exit 5
            ;;
        csv)
            column -s, -t -- "${FILE_PATH}" | head -n "${PV_HEIGHT}" && exit 5
            exit 1;;
        json)
            jq --color-output . "${FILE_PATH}" && exit 5
            python3 -m json.tool -- "${FILE_PATH}" && exit 5
            ;;
        torrent)
            transmission-show -- "${FILE_PATH}" && exit 5; exit 1;;
        ipynb)
            jupyter nbconvert --to script --stdout -- "${FILE_PATH}" 2>/dev/null \
                | bat --color=always --language python && exit 5
            ;;
    esac
}

handle_mime() {
    case "${MIMETYPE}" in
        text/* | */xml | */json | */javascript)
            bat --color=always -- "${FILE_PATH}" && exit 5
            cat -- "${FILE_PATH}" && exit 5
            exit 1;;
        image/*)
            exiftool "${FILE_PATH}" && exit 5
            exit 1;;
        video/* | audio/*)
            exiftool "${FILE_PATH}" && exit 5
            exit 1;;
        application/zip | application/x-*)
            atool --list -- "${FILE_PATH}" && exit 5
            bsdtar --list --file "${FILE_PATH}" && exit 5
            exit 1;;
    esac
}

handle_image
handle_extension
handle_mime
exit 1
