#!/bin/bash

home='/home/paul'
backup_dir='/home/paul/Dropbox/backup'
backup_location='file://'$backup_dir
restore_target='/home/paul/tmp/restoretest'
log_file='/home/paul/logs/backup-log'

# The file specification is a bit complex and difficult to read;
# sorry. I couldn't figure out how to get inline comments to work with
# bash's '\' line continuation. For the most part I specify inclusions
# rather than exclusions. Note that earlier lines take precedence over
# later lines.

echo "Starting backup script $0 at $(date)" >> $log_file

# Do a full backup, logging output and errors
duplicity full \
          --exclude $home/.dbus \
          --exclude $home/.gvfs \
          --exclude $home/.cabal \
          --exclude $home/.cache \
          --exclude $home/.local/share/Trash \
          --exclude $home/.stack \
          --exclude $home/.adobe \
          --include $home/'.*' \
          --include $home/Documents \
          --include $home/dotfiles \
          --include $home/elm \
          --include $home/haskell \
          --include $home/Music \
          --include $home/my-elisp-repos \
          --include $home/notes \
          --include $home/pdfs \
          --include $home/Pictures \
          --include $home/projects \
          --include $home/resume \
          --include $home/sheetmusic \
          --include $home/Videos \
          --exclude '**' \
          --no-encryption \
          $home $backup_location \
          >> $log_file 2>&1

# Only keep the most recent backup, to save space
duplicity remove-all-but-n-full 1 $backup_location --force \
          >> $log_file 2>&1

# Because this script is run in anacron, it's run as root, so the
# backup files it writes only have root permissions. Let's add read
# permissions for everyone so it's easier to copy these around.
chmod a+r $backup_dir/*

# Report collection status
# duplicity collection-status $backup_location

# Restore to somewhere
# duplicity restore $backup_location $restore_target --no-encryption

# Set backup volume permissions so everyone can read them (for some
# reason the default is owner only).


echo "Finishing backup script $0 at $(date)" >> $log_file
