#!/usr/bin/env bash
set -euo pipefail

# ---- Configuration ----------------------------------------------------------

BREW_TAP="d12frosted/emacs-plus"
FORMULA="emacs-plus@31"
EMACS_OPTIONS=(
	"--with-debug"
	"--with-xwidgets"
	"--with-no-frame-refocus"
	"--with-imagemagick"
	"--with-dbus"
)

# If you keep Emacs elsewhere, change this:
APPLICATIONS_DIR="/Applications"
APP_NAME="Emacs.app"

# ---- Helpers ----------------------------------------------------------------

log() { printf "👉 %s\n" "$*"; }

# Prefer /opt/homebrew on Apple Silicon; fallback to /usr/local
detect_brew_prefix() {
	if /usr/bin/arch -arm64 true 2>/dev/null; then
		# On Apple Silicon shells, /opt/homebrew is typical
		if [ -d "/opt/homebrew" ]; then
			echo "/opt/homebrew"
			return
		fi
	fi
	# Intel or custom prefix
	brew --prefix
}

# Resolve the Emacs app path under the keg cellar
find_emacs_app() {
	local prefix="$1"
	local cell_dir="${prefix}/opt/${FORMULA}/${APP_NAME}"
	local alt_cell_dir="${prefix}/Cellar/${FORMULA}"
	if [ -d "${cell_dir}" ]; then
		echo "${cell_dir}"
		return
	fi
	# Fallback: find most recent version in Cellar
	if [ -d "${alt_cell_dir}" ]; then
		local latest
		latest="$(ls -1 "${alt_cell_dir}" | sort -V | tail -n1)"
		if [ -n "${latest}" ] && [ -d "${alt_cell_dir}/${latest}/${APP_NAME}" ]; then
			echo "${alt_cell_dir}/${latest}/${APP_NAME}"
			return
		fi
	fi
	return 1
}

# ---- Main -------------------------------------------------------------------

log "Ensuring Homebrew tap: ${BREW_TAP}"
brew tap "${BREW_TAP}"

log "Uninstalling existing ${FORMULA} (if present)"
# Use --force to remove old keg if linked; ignore errors when not installed
brew uninstall --force "${FORMULA}" || true

log "Installing ${FORMULA} with options: ${EMACS_OPTIONS[*]}"
brew install "${BREW_TAP}/${FORMULA}" "${EMACS_OPTIONS[@]}"

BREW_PREFIX="$(detect_brew_prefix)"
log "Homebrew prefix detected: ${BREW_PREFIX}"

EMACS_APP_PATH="$(find_emacs_app "${BREW_PREFIX}")" || {
	echo "❌ Could not locate ${APP_NAME} inside ${FORMULA} after install."
	echo "   Try: brew info ${FORMULA} and verify the keg, then update this script."
	exit 1
}
log "Found Emacs.app at: ${EMACS_APP_PATH}"


# Resilient deletion + alias creation with fallbacks (AppleScript -> symlink)
TARGET_APP="${APPLICATIONS_DIR}/${APP_NAME}"

log "Preparing ${TARGET_APP} link to ${EMACS_APP_PATH}"

# 1) Remove existing target (regular bundle, symlink, Finder alias)
if [ -e "${TARGET_APP}" ] || [ -L "${TARGET_APP}" ]; then
  log "Deleting existing ${TARGET_APP} (rm -rf)"
  if rm -rf "${TARGET_APP}"; then
    log "Deleted ${TARGET_APP} via rm -rf"
  else
    log "rm -rf failed; attempting Finder delete via AppleScript"
    osascript <<OSA || true
tell application "Finder"
  try
    set appsPOSIX to POSIX file "${APPLICATIONS_DIR}" as alias
    if exists file "${APP_NAME}" of appsPOSIX then
      delete file "${APP_NAME}" of appsPOSIX
    else if exists folder "${APP_NAME}" of appsPOSIX then
      delete folder "${APP_NAME}" of appsPOSIX
    end if
  end try
end tell
OSA
  fi
fi

# 2) Try to create a Finder alias first (preferred)
log "Creating Finder alias to ${EMACS_APP_PATH}"
if osascript <<OSA
tell application "Finder"
  set emacsPOSIX to POSIX file "${EMACS_APP_PATH}" as alias
  set appsPOSIX to POSIX file "${APPLICATIONS_DIR}" as alias
  make alias file to emacsPOSIX at appsPOSIX with properties {name:"${APP_NAME}"}
end tell
OSA
then
  log "Finder alias created: ${TARGET_APP}"
else
  log "Finder alias failed; falling back to symlink"
  if ln -s "${EMACS_APP_PATH}" "${TARGET_APP}"; then
    log "Symlink created: ${TARGET_APP} -> ${EMACS_APP_PATH}"
  else
    echo "❌ Failed to create ${TARGET_APP} (alias and symlink)."
    echo "   You can manually link: ln -s \"${EMACS_APP_PATH}\" \"${TARGET_APP}\""
    exit 1
  fi
fi

# Codesign (self-sign) to suppress macOS task policy / QoS noise
log "Codesigning ${EMACS_APP_PATH} (deep, ad-hoc '-')"
codesign --force --deep --sign - "${EMACS_APP_PATH}"

# Verify codesign status
log "Verifying codesign"
codesign --verify --deep --strict "${EMACS_APP_PATH}" || {
	echo "⚠️ codesign verification reported issues; continuing anyway."
}

# Optional: Gatekeeper assessment (informational)
if command -v spctl >/dev/null 2>&1; then
	log "Gatekeeper assessment (informational)"
	spctl --assess --verbose=3 "${EMACS_APP_PATH}" || true
fi

log "Done. You can run: open -a Emacs  (or the CLI binary inside the bundle)"
