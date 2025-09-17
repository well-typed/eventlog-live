#!/bin/sh -e

# Read the expected version:
CABAL_EXPECT_VERSION="$(awk -F'=' '/^cabal=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal:
#
# 1. Use CABAL if it is set.
# 2. Look for cabal-$EXPECTED_VERSION.
# 3. Look for cabal.
#
if [ "${CABAL}" = "" ]; then
  if ! CABAL="$(which "cabal-${CABAL_EXPECT_VERSION}")"; then
    if ! CABAL="$(which "cabal")"; then
      echo "Requires cabal ${CABAL_EXPECT_VERSION}; no version found"
      exit 1
    fi
  fi
fi

# Check cabal version:
CABAL_ACTUAL_VERSION="$("${CABAL}" --numeric-version | head -n 1)"
if [ "${CABAL_ACTUAL_VERSION}" != "${CABAL_EXPECT_VERSION}" ]; then
  # Version mismatch is never an error:
  echo "Requires cabal ${CABAL_EXPECT_VERSION}; version ${CABAL_ACTUAL_VERSION} found"
fi

# Read the expected version:
HADDOCK_EXPECT_VERSION="$(awk -F'=' '/^haddock=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find haddock:
#
# 1. Use HADDOCK if it is set.
# 2. Look for haddock-$EXPECTED_VERSION.
# 3. Look for haddock.
#
if [ "${HADDOCK}" = "" ]; then
  if ! HADDOCK="$(which "haddock-${HADDOCK_EXPECT_VERSION}")"; then
    if ! HADDOCK="$(which "haddock")"; then
      echo "Requires haddock ${HADDOCK_EXPECT_VERSION}; no version found"
      exit 1
    fi
  fi
fi

# Check haddock version:
HADDOCK_ACTUAL_VERSION="$("${HADDOCK}" --version | head -n1 | cut -d' ' -f3 | cut -d',' -f1)"
if [ "${HADDOCK_ACTUAL_VERSION}" != "${HADDOCK_EXPECT_VERSION}" ]; then
  # Version mismatch is never an error:
  echo "Requires haddock ${HADDOCK_EXPECT_VERSION}; version ${HADDOCK_ACTUAL_VERSION} found"
fi

# Configure metadata
# TODO: read from .cabal file
PKG_VERSION="0.1.0.0"

# Make working directory:
WORKDIR=$(mktemp -d build-haddock.XXXXXX)
trap 'rm -r "${WORKDIR}"' EXIT

# Make build directory:
BUILDDIR="${WORKDIR}/dist-newstyle"

# Make dist directory:
DIST="eventlog-live-${PKG_VERSION}-docs"
DISTDIR="${WORKDIR}/${DIST}"
mkdir "${DISTDIR}"

# Build haddock
"${CABAL}" haddock \
	--builddir="${BUILDDIR}" \
	--report-planning-failure \
	--haddock-hyperlink-source \
	--haddock-html \
	--haddock-hoogle \
	--haddock-quickjump \
	--haddock-option="--use-contents=.." \
	eventlog-live \
	eventlog-live:options \
	eventlog-live:socket

# Handle library
set --  "${BUILDDIR}/build/"*"-"*"/ghc-"*"/eventlog-live-"*"/doc/html/eventlog-live"
for PKG_DIR; do
	cp -r "${PKG_DIR}/" "${DISTDIR}/"
done

# Handle options sub-library
set -- "${BUILDDIR}/build/"*"-"*"/ghc-"*"/eventlog-live-"*"/l/options/doc/html/eventlog-live"
for PKG_OPTIONS_DIR; do
	mv "${PKG_OPTIONS_DIR}/eventlog-live.haddock" "${PKG_OPTIONS_DIR}/options.haddock"
	mv "${PKG_OPTIONS_DIR}/eventlog-live.txt" "${PKG_OPTIONS_DIR}/options.txt"
	cp -r "${PKG_OPTIONS_DIR}/" "${DISTDIR}/"
done

# Handle socket sub-library
set -- "${BUILDDIR}/build/"*"-"*"/ghc-"*"/eventlog-live-"*"/l/socket/doc/html/eventlog-live"
for PKG_SOCKET_DIR; do
	mv "${PKG_SOCKET_DIR}/eventlog-live.haddock" "${PKG_SOCKET_DIR}/socket.haddock"
	mv "${PKG_SOCKET_DIR}/eventlog-live.txt" "${PKG_SOCKET_DIR}/socket.txt"
	cp -r "${PKG_SOCKET_DIR}/" "${DISTDIR}/"
done

# Create prologue
PROLOGUE="${WORKDIR}/prologue.haddock"
"./scripts/generate-haddock-prologue.hs" \
	<"./eventlog-live/eventlog-live.cabal" \
	>"${PROLOGUE}"

# Build HTML and js (quickjump) indexes
"${HADDOCK}" \
  -o "${DISTDIR}" \
  --prologue="${PROLOGUE}" \
  --quickjump \
  --gen-index \
  --gen-contents \
  --read-interface="${DISTDIR}/eventlog-live.haddock" \
  --read-interface="${DISTDIR}/options.haddock" \
  --read-interface="${DISTDIR}/socket.haddock"

# Create tarball for Hackage
tar -czvf "eventlog-live-${PKG_VERSION}-docs.tar.gz" --format="ustar" -C "${WORKDIR}" "${DIST}"
