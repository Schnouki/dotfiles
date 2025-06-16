complete -c sentry-cli -n "__fish_use_subcommand" -l url -d 'Fully qualified URL to the Sentry server.{n}[default: https://sentry.io/]' -r
complete -c sentry-cli -n "__fish_use_subcommand" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_use_subcommand" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_use_subcommand" -l api-key -d 'Use the given Sentry API key.' -r
complete -c sentry-cli -n "__fish_use_subcommand" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_use_subcommand" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_use_subcommand" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_use_subcommand" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_use_subcommand" -s V -l version -d 'Print version'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "completions" -d 'Generate completions for the specified shell.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "bash-hook" -d 'Prints out a bash script that does error handling.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "debug-files" -d 'Locate, analyze or upload debug information files.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "dif" -d 'Locate, analyze or upload debug information files.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "deploys" -d 'Manage deployments for Sentry releases.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "events" -d 'Manage events on Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "files" -d 'Manage release artifacts.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "info" -d 'Print information about the configuration and verify authentication.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "issues" -d 'Manage issues in Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "login" -d 'Authenticate with the Sentry server.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "monitors" -d 'Manage cron monitors on Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "organizations" -d 'Manage organizations on Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "projects" -d 'Manage projects on Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "react-native" -d 'Upload build artifacts for react-native projects.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "releases" -d 'Manage releases on Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "repos" -d 'Manage repositories on Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "send-event" -d 'Send a manual event to Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "send-envelope" -d 'Send a stored envelope to Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "send-metric" -d 'Send a metric to Sentry.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "sourcemaps" -d 'Manage sourcemaps for Sentry releases.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "uninstall" -d 'Uninstall the sentry-cli executable.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "update" -d 'Update the sentry-cli executable.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "upload-dif" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "upload-dsym" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "upload-proguard" -d 'Upload ProGuard mapping files to a project.'
complete -c sentry-cli -n "__fish_use_subcommand" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from completions" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from completions" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from completions" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from completions" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from completions" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from completions" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l cli -d 'Explicitly set/override the sentry-cli command' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l traceback -r
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l tag -d 'Add tags (key:value) to the event.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l release -d 'Define release version for the event.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l log -r
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l no-exit -d 'Do not turn on -e (exit immediately) flag automatically'
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l no-environ -d 'Do not send environment variables along'
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l send-event
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from bash-hook" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-sources" -d 'Create a source bundle for a given debug information file'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "check" -d 'Check the debug info file at a given path.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-jvm" -d 'Create a source bundle for the given JVM based source files (e.g. Java, Kotlin, ...)'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "find" -d 'Locate debug information files for given debug identifiers.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "print-sources" -d 'Print source files linked by the given debug info file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "upload" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-sources" -s o -l output -d 'The path to the output folder.  If not provided the file is placed next to the input file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-sources" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-sources" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-sources" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-sources" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-sources" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-sources" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -s t -l type -d 'Explicitly set the type of the debug info file. This should not be needed as files are auto detected.' -r -f -a "{dsym\t'',elf\t'',pe\t'',pdb\t'',portablepdb\t'',sourcebundle\t'',breakpad\t'',proguard\t'',wasm\t'',jvm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -l json -d 'Format outputs as JSON.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files check" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -l output -d 'The path to the output folder.' -r -F
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -l debug-id -d 'Debug ID (UUID) to use for the source bundle.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files bundle-jvm" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -s t -l type -d 'Only consider debug information files of the given type.  By default all types are considered.' -r -f -a "{dsym\t'',elf\t'',pe\t'',pdb\t'',portablepdb\t'',sourcebundle\t'',breakpad\t'',proguard\t'',wasm\t'',jvm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -s p -l path -d 'Add a path to search recursively for debug info files.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l no-well-known -d 'Do not look for debug symbols in well known locations.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l no-cwd -d 'Do not look for debug symbols in the current working directory.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l json -d 'Format outputs as JSON.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files find" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files print-sources" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files print-sources" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files print-sources" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files print-sources" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files print-sources" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files print-sources" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -s t -l type -d 'Only consider debug information files of the given type.  By default, all types are considered.' -r -f -a "{bcsymbolmap\t'',breakpad\t'',dsym\t'',elf\t'',jvm\t'',pdb\t'',pe\t'',portablepdb\t'',sourcebundle\t'',wasm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l id -d 'Search for specific debug identifiers.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l symbol-maps -d 'Optional path to BCSymbolMap files which are used to resolve hidden symbols in dSYM files downloaded from iTunes Connect.  This requires the dsymutil tool to be available.  This should not be used when using the App Store Connect integration, the .bcsymbolmap files needed for the integration are uploaded without this option if they are found in the PATH searched for symbol files.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l info-plist -d 'Optional path to the Info.plist.{n}We will try to find this automatically if run from Xcode.  Providing this information will associate the debug symbols with a specific ITC application and build in Sentry.  Note that if you provide the plist explicitly it must already be processed.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l no-unwind -d 'Do not scan for stack unwinding information. Specify this flag for builds with disabled FPO, or when stackwalking occurs on the device. This usually excludes executables and dynamic libraries. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l no-debug -d 'Do not scan for debugging information. This will usually exclude debug companion files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l no-sources -d 'Do not scan for source information. This will usually exclude source bundle files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l require-all -d 'Errors if not all identifiers specified with --id could be found.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l derived-data -d 'Search for debug symbols in Xcode\'s derived data.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l no-zips -d 'Do not search in ZIP files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l no-upload -d 'Disable the actual upload.{n}This runs all steps for the processing but does not trigger the upload.  This is useful if you just want to verify the setup or skip the upload in tests.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l force-foreground -d 'Wait for the process to finish.{n}By default, the upload process will detach and continue in the background when triggered from Xcode.  When an error happens, a dialog is shown.  If this parameter is passed Xcode will wait for the process to finish before the build finishes and output will be shown in the Xcode build output.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l include-sources -d 'Include sources from the local file system and upload them as source bundles.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l wait -d 'Wait for the server to fully process uploaded files. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l upload-symbol-maps
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l il2cpp-mapping -d 'Compute il2cpp line mappings and upload them along with sources.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files upload" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-sources" -d 'Create a source bundle for a given debug information file'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "check" -d 'Check the debug info file at a given path.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-jvm" -d 'Create a source bundle for the given JVM based source files (e.g. Java, Kotlin, ...)'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "find" -d 'Locate debug information files for given debug identifiers.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "print-sources" -d 'Print source files linked by the given debug info file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "upload" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from debug-files help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-sources" -d 'Create a source bundle for a given debug information file'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "check" -d 'Check the debug info file at a given path.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-jvm" -d 'Create a source bundle for the given JVM based source files (e.g. Java, Kotlin, ...)'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "find" -d 'Locate debug information files for given debug identifiers.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "print-sources" -d 'Print source files linked by the given debug info file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "upload" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-sources" -s o -l output -d 'The path to the output folder.  If not provided the file is placed next to the input file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-sources" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-sources" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-sources" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-sources" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-sources" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-sources" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -s t -l type -d 'Explicitly set the type of the debug info file. This should not be needed as files are auto detected.' -r -f -a "{dsym\t'',elf\t'',pe\t'',pdb\t'',portablepdb\t'',sourcebundle\t'',breakpad\t'',proguard\t'',wasm\t'',jvm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -l json -d 'Format outputs as JSON.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif check" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -l output -d 'The path to the output folder.' -r -F
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -l debug-id -d 'Debug ID (UUID) to use for the source bundle.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif bundle-jvm" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -s t -l type -d 'Only consider debug information files of the given type.  By default all types are considered.' -r -f -a "{dsym\t'',elf\t'',pe\t'',pdb\t'',portablepdb\t'',sourcebundle\t'',breakpad\t'',proguard\t'',wasm\t'',jvm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -s p -l path -d 'Add a path to search recursively for debug info files.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l no-well-known -d 'Do not look for debug symbols in well known locations.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l no-cwd -d 'Do not look for debug symbols in the current working directory.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l json -d 'Format outputs as JSON.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif find" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif print-sources" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif print-sources" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif print-sources" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif print-sources" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif print-sources" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif print-sources" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -s t -l type -d 'Only consider debug information files of the given type.  By default, all types are considered.' -r -f -a "{bcsymbolmap\t'',breakpad\t'',dsym\t'',elf\t'',jvm\t'',pdb\t'',pe\t'',portablepdb\t'',sourcebundle\t'',wasm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l id -d 'Search for specific debug identifiers.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l symbol-maps -d 'Optional path to BCSymbolMap files which are used to resolve hidden symbols in dSYM files downloaded from iTunes Connect.  This requires the dsymutil tool to be available.  This should not be used when using the App Store Connect integration, the .bcsymbolmap files needed for the integration are uploaded without this option if they are found in the PATH searched for symbol files.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l info-plist -d 'Optional path to the Info.plist.{n}We will try to find this automatically if run from Xcode.  Providing this information will associate the debug symbols with a specific ITC application and build in Sentry.  Note that if you provide the plist explicitly it must already be processed.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l no-unwind -d 'Do not scan for stack unwinding information. Specify this flag for builds with disabled FPO, or when stackwalking occurs on the device. This usually excludes executables and dynamic libraries. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l no-debug -d 'Do not scan for debugging information. This will usually exclude debug companion files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l no-sources -d 'Do not scan for source information. This will usually exclude source bundle files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l require-all -d 'Errors if not all identifiers specified with --id could be found.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l derived-data -d 'Search for debug symbols in Xcode\'s derived data.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l no-zips -d 'Do not search in ZIP files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l no-upload -d 'Disable the actual upload.{n}This runs all steps for the processing but does not trigger the upload.  This is useful if you just want to verify the setup or skip the upload in tests.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l force-foreground -d 'Wait for the process to finish.{n}By default, the upload process will detach and continue in the background when triggered from Xcode.  When an error happens, a dialog is shown.  If this parameter is passed Xcode will wait for the process to finish before the build finishes and output will be shown in the Xcode build output.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l include-sources -d 'Include sources from the local file system and upload them as source bundles.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l wait -d 'Wait for the server to fully process uploaded files. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l upload-symbol-maps
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l il2cpp-mapping -d 'Compute il2cpp line mappings and upload them along with sources.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif upload" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-sources" -d 'Create a source bundle for a given debug information file'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "check" -d 'Check the debug info file at a given path.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "bundle-jvm" -d 'Create a source bundle for the given JVM based source files (e.g. Java, Kotlin, ...)'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "find" -d 'Locate debug information files for given debug identifiers.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "print-sources" -d 'Print source files linked by the given debug info file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "upload" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from dif help; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -f -a "list" -d 'List all deployments of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -f -a "new" -d 'Creates a new release deployment.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys; and not __fish_seen_subcommand_from list new help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s e -l env -d 'Set the environment for this release.{n}This argument is required.  Values that make sense here would be \'production\' or \'staging\'.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s n -l name -d 'Optional human readable name for this deployment.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s u -l url -d 'Optional URL that points to the deployment.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l started -d 'Optional unix timestamp when the deployment started.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l finished -d 'Optional unix timestamp when the deployment finished.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s t -l time -d 'Optional deployment duration in seconds.{n}This can be specified alternatively to `--started` and `--finished`.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys new" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys help; and not __fish_seen_subcommand_from list new help" -f -a "list" -d 'List all deployments of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys help; and not __fish_seen_subcommand_from list new help" -f -a "new" -d 'Creates a new release deployment.'
complete -c sentry-cli -n "__fish_seen_subcommand_from deploys help; and not __fish_seen_subcommand_from list new help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all events in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -l max-rows -d 'Maximum number of rows to print.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -l pages -d 'Maximum number of pages to fetch (100 events/page).' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -s U -l show-user -d 'Display the Users column.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -s T -l show-tags -d 'Display the Tags column.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from events help; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all events in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from events help; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "upload-sourcemaps" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "delete" -d 'Delete a release file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "list" -d 'List all release files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "upload" -d 'Upload files for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s u -l url-prefix -d 'The URL prefix to prepend to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l url-suffix -d 'The URL suffix to append to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s d -l dist -d 'Optional distribution identifier for the sourcemaps.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l note -d 'Adds an optional note to the uploaded artifact bundle.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l strip-prefix -d 'Strips the given prefix from all sources references inside the upload sourcemaps (paths used within the sourcemap content, to map minified code to it\'s original source). Only sources that start with the given prefix will be stripped.{n}This will not modify the uploaded sources paths. To do that, point the upload or upload-sourcemaps command to a more precise directory instead.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s i -l ignore -d 'Ignores all files and folders matching the given glob' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s I -l ignore-file -d 'Ignore all files and folders specified in the given ignore file, e.g. .gitignore.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l bundle -d 'Path to the application bundle (indexed, file, or regular)' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l bundle-sourcemap -d 'Path to the bundle sourcemap' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s x -l ext -d 'Set the file extensions that are considered for upload. This overrides the default extensions. To add an extension, all default extensions must be repeated. Specify once per extension. Defaults to: `--ext=js --ext=cjs --ext=mjs --ext=map --ext=jsbundle --ext=bundle`' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l validate -d 'Enable basic sourcemap validation.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l decompress -d 'Enable files gzip decompression prior to upload.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l wait -d 'Wait for the server to fully process uploaded files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l no-sourcemap-reference -d 'Disable emitting of automatic sourcemap references.{n}By default the tool will store a \'Sourcemap\' header with minified files so that sourcemaps are located automatically if the tool can detect a link. If this causes issues it can be disabled.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l debug-id-reference -d 'Enable emitting of automatic debug id references.{n}By default Debug ID reference has to be present both in the source and the related sourcemap. But in cases of binary bundles, the tool can\'t verify presence of the Debug ID. This flag allows use of Debug ID from the linked sourcemap.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l no-rewrite -d 'Disables rewriting of matching sourcemaps. By default the tool will rewrite sources, so that indexed maps are flattened and missing sources are inlined if possible.{n}This fundamentally changes the upload process to be based on sourcemaps and minified files exclusively and comes in handy for setups like react-native that generate sourcemaps that would otherwise not work for sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l strip-common-prefix -d 'Similar to --strip-prefix but strips the most common prefix on all sources references.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l no-dedupe -d 'Skip artifacts deduplication prior to uploading. This will force all artifacts to be uploaded, no matter whether they are already present on the server.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s s -l strict -d 'Fail with a non-zero exit code if the specified source map file cannot be uploaded.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l use-artifact-bundle -d 'Use new Artifact Bundles upload, that enables the use of Debug IDs for Source Maps discovery.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l rewrite
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s v -l verbose
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload-sourcemaps" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -s A -l all -d 'Delete all files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files delete" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s d -l dist -d 'Optional distribution identifier for this file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s H -l file-header -d 'Store a header with this file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s u -l url-prefix -d 'The URL prefix to prepend to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l url-suffix -d 'The URL suffix to append to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s i -l ignore -d 'Ignores all files and folders matching the given glob' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s I -l ignore-file -d 'Ignore all files and folders specified in the given ignore file, e.g. .gitignore.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s x -l ext -d 'Set the file extensions that are considered for upload. This overrides the default extensions. To add an extension, all default extensions must be repeated. Specify once per extension.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l decompress -d 'Enable files gzip decompression prior to upload.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l wait -d 'Wait for the server to fully process uploaded files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files upload" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "upload-sourcemaps" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "delete" -d 'Delete a release file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "list" -d 'List all release files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "upload" -d 'Upload files for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -l config-status-json -d 'Return the status of the config that sentry-cli loads as JSON dump. This can be used by external tools to aid the user towards configuration.'
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -l no-defaults -d 'Skip default organization and project checks. This allows you to verify your authentication method, without the need for setting other defaults.'
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from info" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -s s -l status -d 'Select all issues matching a given status.' -r -f -a "{resolved\t'',muted\t'',unresolved\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -s i -l id -d 'Select the issue with the given ID.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -s a -l all -d 'Select all issues (this might be limited).'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "list" -d 'List all issues in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "mute" -d 'Bulk mute all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "resolve" -d 'Bulk resolve all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "unresolve" -d 'Bulk unresolve all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l max-rows -d 'Maximum number of rows to print.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l pages -d 'Maximum number of pages to fetch (100 issues/page).' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l query -d 'Query to pass at the request. An example is "is:unresolved"' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -s s -l status -d 'Select all issues matching a given status.' -r -f -a "{resolved\t'',muted\t'',unresolved\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -s i -l id -d 'Select the issue with the given ID.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -s a -l all -d 'Select all issues (this might be limited).'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -s s -l status -d 'Select all issues matching a given status.' -r -f -a "{resolved\t'',muted\t'',unresolved\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -s i -l id -d 'Select the issue with the given ID.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -s a -l all -d 'Select all issues (this might be limited).'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues mute" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -s s -l status -d 'Select all issues matching a given status.' -r -f -a "{resolved\t'',muted\t'',unresolved\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -s i -l id -d 'Select the issue with the given ID.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -s n -l next-release -d 'Only select issues in the next release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -s a -l all -d 'Select all issues (this might be limited).'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues resolve" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -s s -l status -d 'Select all issues matching a given status.' -r -f -a "{resolved\t'',muted\t'',unresolved\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -s i -l id -d 'Select the issue with the given ID.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -s a -l all -d 'Select all issues (this might be limited).'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues unresolve" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues help; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "list" -d 'List all issues in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues help; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "mute" -d 'Bulk mute all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues help; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "resolve" -d 'Bulk resolve all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues help; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "unresolve" -d 'Bulk unresolve all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from issues help; and not __fish_seen_subcommand_from list mute resolve unresolve help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from login" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from login" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from login" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from login" -s g -l global -d 'Store authentication token globally rather than locally.'
complete -c sentry-cli -n "__fish_seen_subcommand_from login" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from login" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from login" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -f -a "list" -d 'List all monitors for an organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -f -a "run" -d 'Wraps a command'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors; and not __fish_seen_subcommand_from list run help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -s e -l environment -d 'Specify the environment of the monitor.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -s s -l schedule -d 'Configure the cron monitor with the given schedule (crontab format). Enclose the schedule in quotes to ensure your command line environment parses the argument correctly.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l check-in-margin -d 'The allowed margin of minutes after the expected check-in time that the monitor will not be considered missed for. Requires --schedule.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l max-runtime -d 'The allowed duration in minutes that the monitor may be in progress for before being considered failed due to timeout. Requires --schedule.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l timezone -d 'A tz database string (e.g. "Europe/Vienna") representing the monitor\'s execution schedule\'s timezone. Requires --schedule.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l failure-issue-threshold -d 'The number of consecutive missed or error check-ins that trigger an issue. Requires --schedule.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l recovery-threshold -d 'The number of consecutive successful check-ins that resolve an issue. Requires --schedule.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l auth-token -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors run" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors help; and not __fish_seen_subcommand_from list run help" -f -a "list" -d 'List all monitors for an organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors help; and not __fish_seen_subcommand_from list run help" -f -a "run" -d 'Wraps a command'
complete -c sentry-cli -n "__fish_seen_subcommand_from monitors help; and not __fish_seen_subcommand_from list run help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all organizations available to the authenticated token.'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations help; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all organizations available to the authenticated token.'
complete -c sentry-cli -n "__fish_seen_subcommand_from organizations help; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all projects for an organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from projects list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from projects list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from projects list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from projects list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects help; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all projects for an organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from projects help; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -f -a "gradle" -d 'Upload react-native projects in a gradle build step.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -f -a "appcenter" -d 'Upload react-native projects for AppCenter.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native; and not __fish_seen_subcommand_from gradle appcenter help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l sourcemap -d 'The path to a sourcemap that should be uploaded.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l bundle -d 'The path to a bundle that should be uploaded.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l release -d 'The name of the release to publish.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l dist -d 'The names of the distributions to publish. Can be supplied multiple times.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l wait -d 'Wait for the server to fully process uploaded files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native gradle" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l deployment -d 'The name of the deployment. [Production, Staging]' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l bundle-id -d 'Explicitly provide the bundle ID instead of parsing the source projects.  This allows you to push codepush releases for iOS on platforms without Xcode or codepush releases for Android when you use different bundle IDs for release and debug etc.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l version-name -d 'Override version name in release name' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l dist -d 'The names of the distributions to publish. Can be supplied multiple times.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l release-name -d 'Override the entire release-name' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l print-release-name -d 'Print the release name instead.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l wait -d 'Wait for the server to fully process uploaded files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native appcenter" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native help; and not __fish_seen_subcommand_from gradle appcenter help" -f -a "gradle" -d 'Upload react-native projects in a gradle build step.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native help; and not __fish_seen_subcommand_from gradle appcenter help" -f -a "appcenter" -d 'Upload react-native projects for AppCenter.'
complete -c sentry-cli -n "__fish_seen_subcommand_from react-native help; and not __fish_seen_subcommand_from gradle appcenter help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "files" -d 'Manage release artifacts.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "deploys" -d 'Manage deployments for Sentry releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "archive" -d 'Archive a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "delete" -d 'Delete a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "finalize" -d 'Mark a release as finalized and released.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "info" -d 'Print information about a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "list" -d 'List the most recent releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "new" -d 'Create a new release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "propose-version" -d 'Propose a version name for a new release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "restore" -d 'Restore a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "set-commits" -d 'Set commits of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -a "upload-sourcemaps" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -a "delete" -d 'Delete a release file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -a "list" -d 'List all release files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -a "upload" -d 'Upload files for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s u -l url-prefix -d 'The URL prefix to prepend to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l url-suffix -d 'The URL suffix to append to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s d -l dist -d 'Optional distribution identifier for the sourcemaps.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l note -d 'Adds an optional note to the uploaded artifact bundle.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l strip-prefix -d 'Strips the given prefix from all sources references inside the upload sourcemaps (paths used within the sourcemap content, to map minified code to it\'s original source). Only sources that start with the given prefix will be stripped.{n}This will not modify the uploaded sources paths. To do that, point the upload or upload-sourcemaps command to a more precise directory instead.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s i -l ignore -d 'Ignores all files and folders matching the given glob' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s I -l ignore-file -d 'Ignore all files and folders specified in the given ignore file, e.g. .gitignore.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l bundle -d 'Path to the application bundle (indexed, file, or regular)' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l bundle-sourcemap -d 'Path to the bundle sourcemap' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s x -l ext -d 'Set the file extensions that are considered for upload. This overrides the default extensions. To add an extension, all default extensions must be repeated. Specify once per extension. Defaults to: `--ext=js --ext=cjs --ext=mjs --ext=map --ext=jsbundle --ext=bundle`' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l validate -d 'Enable basic sourcemap validation.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l decompress -d 'Enable files gzip decompression prior to upload.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l wait -d 'Wait for the server to fully process uploaded files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l no-sourcemap-reference -d 'Disable emitting of automatic sourcemap references.{n}By default the tool will store a \'Sourcemap\' header with minified files so that sourcemaps are located automatically if the tool can detect a link. If this causes issues it can be disabled.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l debug-id-reference -d 'Enable emitting of automatic debug id references.{n}By default Debug ID reference has to be present both in the source and the related sourcemap. But in cases of binary bundles, the tool can\'t verify presence of the Debug ID. This flag allows use of Debug ID from the linked sourcemap.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l no-rewrite -d 'Disables rewriting of matching sourcemaps. By default the tool will rewrite sources, so that indexed maps are flattened and missing sources are inlined if possible.{n}This fundamentally changes the upload process to be based on sourcemaps and minified files exclusively and comes in handy for setups like react-native that generate sourcemaps that would otherwise not work for sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l strip-common-prefix -d 'Similar to --strip-prefix but strips the most common prefix on all sources references.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l no-dedupe -d 'Skip artifacts deduplication prior to uploading. This will force all artifacts to be uploaded, no matter whether they are already present on the server.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s s -l strict -d 'Fail with a non-zero exit code if the specified source map file cannot be uploaded.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l use-artifact-bundle -d 'Use new Artifact Bundles upload, that enables the use of Debug IDs for Source Maps discovery.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l rewrite
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s v -l verbose
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload-sourcemaps" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -s A -l all -d 'Delete all files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files delete" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s d -l dist -d 'Optional distribution identifier for this file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s H -l file-header -d 'Store a header with this file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s u -l url-prefix -d 'The URL prefix to prepend to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l url-suffix -d 'The URL suffix to append to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s i -l ignore -d 'Ignores all files and folders matching the given glob' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s I -l ignore-file -d 'Ignore all files and folders specified in the given ignore file, e.g. .gitignore.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s x -l ext -d 'Set the file extensions that are considered for upload. This overrides the default extensions. To add an extension, all default extensions must be repeated. Specify once per extension.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l decompress -d 'Enable files gzip decompression prior to upload.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l wait -d 'Wait for the server to fully process uploaded files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files upload" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "upload-sourcemaps" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "delete" -d 'Delete a release file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "list" -d 'List all release files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "upload" -d 'Upload files for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases files help; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -a "list" -d 'List all deployments of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -a "new" -d 'Creates a new release deployment.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys; and not __fish_seen_subcommand_from list new help" -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s e -l env -d 'Set the environment for this release.{n}This argument is required.  Values that make sense here would be \'production\' or \'staging\'.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s n -l name -d 'Optional human readable name for this deployment.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s u -l url -d 'Optional URL that points to the deployment.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l started -d 'Optional unix timestamp when the deployment started.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l finished -d 'Optional unix timestamp when the deployment finished.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s t -l time -d 'Optional deployment duration in seconds.{n}This can be specified alternatively to `--started` and `--finished`.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys new" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys help; and not __fish_seen_subcommand_from list new help" -f -a "list" -d 'List all deployments of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys help; and not __fish_seen_subcommand_from list new help" -f -a "new" -d 'Creates a new release deployment.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases deploys help; and not __fish_seen_subcommand_from list new help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases archive" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases delete" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l url -d 'Optional URL to the release for information purposes.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l started -d 'Set the release start date.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l released -d 'Set the release time. [defaults to the current time]' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases finalize" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -s P -l show-projects -d 'Display the Projects column'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -s C -l show-commits -d 'Display the Commits column'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases info" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -s D -l delimiter -d 'Delimiter for the --raw flag' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -s P -l show-projects -d 'Display the Projects column'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -s R -l raw -d 'Print raw, delimiter separated list of releases. [defaults to new line]'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -l no-abbrev
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l url -d 'Optional URL to the release for information purposes.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l ref -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l finalize -d 'Immediately finalize the release. (sets it to released)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases new" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases propose-version" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases restore" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l initial-depth -d 'Set the number of commits of the initial release. The default is 20.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -s c -l commit -d 'Defines a single commit for a repo as identified by the repo name in the remote Sentry config. If no commit has been specified sentry-cli will attempt to auto discover that repository in the local git repo and then use the HEAD commit.  This will either use the current git repository or attempt to auto discover a submodule with a compatible URL.  The value can be provided as `REPO` in which case sentry-cli will auto-discover the commit based on reachable repositories. Alternatively it can be provided as `REPO#PATH` in which case the current commit of the repository at the given PATH is assumed.  To override the revision `@REV` can be appended which will force the revision to a certain value.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l clear -d 'Clear all current commits from the release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l auto -d 'Enable completely automated commit management.{n}This requires that the command is run from within a git repository.  sentry-cli will then automatically find remotely configured repositories and discover commits.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l ignore-missing -d 'When the flag is set and the previous release commit was not found in the repository, will create a release with the default commits count (or the one specified with `--initial-depth`) instead of failing the command.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l local -d 'Set commits of a release from local git.{n}This requires that the command is run from within a git repository.  sentry-cli will then automatically find remotely configured repositories and discover commits.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l ignore-empty
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases set-commits" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "files" -d 'Manage release artifacts.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "deploys" -d 'Manage deployments for Sentry releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "archive" -d 'Archive a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "delete" -d 'Delete a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "finalize" -d 'Mark a release as finalized and released.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "info" -d 'Print information about a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "list" -d 'List the most recent releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "new" -d 'Create a new release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "propose-version" -d 'Propose a version name for a new release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "restore" -d 'Restore a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "set-commits" -d 'Set commits of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "upload-sourcemaps" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "delete" -d 'Delete a release file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "list" -d 'List all release files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "upload" -d 'Upload files for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help deploys; and not __fish_seen_subcommand_from list new" -f -a "list" -d 'List all deployments of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from releases help deploys; and not __fish_seen_subcommand_from list new" -f -a "new" -d 'Creates a new release deployment.'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all repositories in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos list" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from repos list" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from repos list" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from repos list" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from repos list" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos list" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos list" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos help; and not __fish_seen_subcommand_from list help" -f -a "list" -d 'List all repositories in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from repos help; and not __fish_seen_subcommand_from list help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s l -l level -d 'Optional event severity/log level. (debug|info|warning|error|fatal) [defaults to \'error\']' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l timestamp -d 'Optional event timestamp in one of supported formats: unix timestamp, RFC2822 or RFC3339.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s r -l release -d 'Optional identifier of the release.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s d -l dist -d 'Set the distribution.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s E -l env -d 'Send with a specific environment.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s m -l message -d 'The event message.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s a -l message-arg -d 'Arguments for the event message.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s p -l platform -d 'Override the default \'other\' platform specifier.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s t -l tag -d 'Add a tag (key:value) to the event.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s e -l extra -d 'Add extra information (key:value) to the event.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s u -l user -d 'Add user information (key:value) to the event. [eg: id:42, username:foo]' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s f -l fingerprint -d 'Change the fingerprint of the event.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l logfile -d 'Send a logfile as breadcrumbs with the event (last 100 records)' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l raw -d 'Send events using an envelope without attempting to parse their contents.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l no-environ -d 'Do not send environment variables along'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l with-categories -d 'Parses off a leading category for breadcrumbs from the logfile'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-event" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-envelope" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-envelope" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-envelope" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from send-envelope" -l raw -d 'Send envelopes without attempting to parse their contents.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-envelope" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-envelope" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-envelope" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "increment" -d 'Increment a counter metric'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "distribution" -d 'Update a distribution metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "gauge" -d 'Update a gauge metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "set" -d 'Update a set metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -s n -l name -d 'The name of the metric, identifying it in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -s u -l unit -d 'Any custom unit. You can have multiple metrics with the same name but different units.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -s t -l tags -d 'Metric tags as key:value pairs. Tags allow you to add dimensions to your metrics and can be filtered or grouped by in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -s v -l value -d 'Value to increment the metric by, any finite 64 bit float.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric increment" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -s n -l name -d 'The name of the metric, identifying it in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -s u -l unit -d 'Any custom unit. You can have multiple metrics with the same name but different units.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -s t -l tags -d 'Metric tags as key:value pairs. Tags allow you to add dimensions to your metrics and can be filtered or grouped by in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -s v -l value -d 'Metric value, any finite 64 bit float.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric distribution" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -s n -l name -d 'The name of the metric, identifying it in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -s u -l unit -d 'Any custom unit. You can have multiple metrics with the same name but different units.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -s t -l tags -d 'Metric tags as key:value pairs. Tags allow you to add dimensions to your metrics and can be filtered or grouped by in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -s v -l value -d 'Metric value, any finite 64 bit float.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric gauge" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -s n -l name -d 'The name of the metric, identifying it in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -s u -l unit -d 'Any custom unit. You can have multiple metrics with the same name but different units.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -s t -l tags -d 'Metric tags as key:value pairs. Tags allow you to add dimensions to your metrics and can be filtered or grouped by in Sentry.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -s v -l value -d 'Value to add to the set. If the set already contains the provided value, the set\'s unique count will not increase.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric set" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric help; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "increment" -d 'Increment a counter metric'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric help; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "distribution" -d 'Update a distribution metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric help; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "gauge" -d 'Update a gauge metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric help; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "set" -d 'Update a set metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from send-metric help; and not __fish_seen_subcommand_from increment distribution gauge set help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "explain" -d 'Explain why sourcemaps are not working for a given event.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "inject" -d 'Fixes up JavaScript source files and sourcemaps with debug ids.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "resolve" -d 'Resolve sourcemap for a given line/column position.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "upload" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -l frame -d 'Position of the frame that should be used for source map resolution.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -s f -l force -d 'Force full validation flow, even when event is already source mapped.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps explain" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -s i -l ignore -d 'Ignores all files and folders matching the given glob' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -s I -l ignore-file -d 'Ignore all files and folders specified in the given ignore file, e.g. .gitignore.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -s x -l ext -d 'Set the file extensions of JavaScript files that are considered for injection.  This overrides the default extensions (js, cjs, mjs).  To add an extension, all default extensions must be repeated.  Specify once per extension.  Source maps are discovered via those files.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -l dry-run -d 'Don\'t modify files on disk.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps inject" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -s l -l line -d 'Line number for minified source.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -s c -l column -d 'Column number for minified source.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps resolve" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l version -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s u -l url-prefix -d 'The URL prefix to prepend to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l url-suffix -d 'The URL suffix to append to all filenames.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s d -l dist -d 'Optional distribution identifier for the sourcemaps.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l note -d 'Adds an optional note to the uploaded artifact bundle.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l strip-prefix -d 'Strips the given prefix from all sources references inside the upload sourcemaps (paths used within the sourcemap content, to map minified code to it\'s original source). Only sources that start with the given prefix will be stripped.{n}This will not modify the uploaded sources paths. To do that, point the upload or upload-sourcemaps command to a more precise directory instead.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s i -l ignore -d 'Ignores all files and folders matching the given glob' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s I -l ignore-file -d 'Ignore all files and folders specified in the given ignore file, e.g. .gitignore.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l bundle -d 'Path to the application bundle (indexed, file, or regular)' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l bundle-sourcemap -d 'Path to the bundle sourcemap' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s x -l ext -d 'Set the file extensions that are considered for upload. This overrides the default extensions. To add an extension, all default extensions must be repeated. Specify once per extension. Defaults to: `--ext=js --ext=cjs --ext=mjs --ext=map --ext=jsbundle --ext=bundle`' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s r -l release -d 'The release slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l validate -d 'Enable basic sourcemap validation.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l decompress -d 'Enable files gzip decompression prior to upload.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l wait -d 'Wait for the server to fully process uploaded files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l no-sourcemap-reference -d 'Disable emitting of automatic sourcemap references.{n}By default the tool will store a \'Sourcemap\' header with minified files so that sourcemaps are located automatically if the tool can detect a link. If this causes issues it can be disabled.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l debug-id-reference -d 'Enable emitting of automatic debug id references.{n}By default Debug ID reference has to be present both in the source and the related sourcemap. But in cases of binary bundles, the tool can\'t verify presence of the Debug ID. This flag allows use of Debug ID from the linked sourcemap.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l no-rewrite -d 'Disables rewriting of matching sourcemaps. By default the tool will rewrite sources, so that indexed maps are flattened and missing sources are inlined if possible.{n}This fundamentally changes the upload process to be based on sourcemaps and minified files exclusively and comes in handy for setups like react-native that generate sourcemaps that would otherwise not work for sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l strip-common-prefix -d 'Similar to --strip-prefix but strips the most common prefix on all sources references.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l no-dedupe -d 'Skip artifacts deduplication prior to uploading. This will force all artifacts to be uploaded, no matter whether they are already present on the server.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s s -l strict -d 'Fail with a non-zero exit code if the specified source map file cannot be uploaded.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l use-artifact-bundle -d 'Use new Artifact Bundles upload, that enables the use of Debug IDs for Source Maps discovery.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l rewrite
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s v -l verbose
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps upload" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps help; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "explain" -d 'Explain why sourcemaps are not working for a given event.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps help; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "inject" -d 'Fixes up JavaScript source files and sourcemaps with debug ids.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps help; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "resolve" -d 'Resolve sourcemap for a given line/column position.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps help; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "upload" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from sourcemaps help; and not __fish_seen_subcommand_from explain inject resolve upload help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from uninstall" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from uninstall" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from uninstall" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from uninstall" -l confirm -d 'Skip uninstall confirmation prompt.'
complete -c sentry-cli -n "__fish_seen_subcommand_from uninstall" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from uninstall" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from uninstall" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from update" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from update" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from update" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from update" -s f -l force -d 'Force the update even if the latest version is already installed.'
complete -c sentry-cli -n "__fish_seen_subcommand_from update" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from update" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from update" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -s t -l type -d 'Only consider debug information files of the given type.  By default, all types are considered.' -r -f -a "{bcsymbolmap\t'',breakpad\t'',dsym\t'',elf\t'',jvm\t'',pdb\t'',pe\t'',portablepdb\t'',sourcebundle\t'',wasm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l id -d 'Search for specific debug identifiers.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l symbol-maps -d 'Optional path to BCSymbolMap files which are used to resolve hidden symbols in dSYM files downloaded from iTunes Connect.  This requires the dsymutil tool to be available.  This should not be used when using the App Store Connect integration, the .bcsymbolmap files needed for the integration are uploaded without this option if they are found in the PATH searched for symbol files.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l info-plist -d 'Optional path to the Info.plist.{n}We will try to find this automatically if run from Xcode.  Providing this information will associate the debug symbols with a specific ITC application and build in Sentry.  Note that if you provide the plist explicitly it must already be processed.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l no-unwind -d 'Do not scan for stack unwinding information. Specify this flag for builds with disabled FPO, or when stackwalking occurs on the device. This usually excludes executables and dynamic libraries. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l no-debug -d 'Do not scan for debugging information. This will usually exclude debug companion files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l no-sources -d 'Do not scan for source information. This will usually exclude source bundle files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l require-all -d 'Errors if not all identifiers specified with --id could be found.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l derived-data -d 'Search for debug symbols in Xcode\'s derived data.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l no-zips -d 'Do not search in ZIP files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l no-upload -d 'Disable the actual upload.{n}This runs all steps for the processing but does not trigger the upload.  This is useful if you just want to verify the setup or skip the upload in tests.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l force-foreground -d 'Wait for the process to finish.{n}By default, the upload process will detach and continue in the background when triggered from Xcode.  When an error happens, a dialog is shown.  If this parameter is passed Xcode will wait for the process to finish before the build finishes and output will be shown in the Xcode build output.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l include-sources -d 'Include sources from the local file system and upload them as source bundles.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l wait -d 'Wait for the server to fully process uploaded files. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l upload-symbol-maps
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l il2cpp-mapping -d 'Compute il2cpp line mappings and upload them along with sources.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dif" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -s t -l type -d 'Only consider debug information files of the given type.  By default, all types are considered.' -r -f -a "{bcsymbolmap\t'',breakpad\t'',dsym\t'',elf\t'',jvm\t'',pdb\t'',pe\t'',portablepdb\t'',sourcebundle\t'',wasm\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l id -d 'Search for specific debug identifiers.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l symbol-maps -d 'Optional path to BCSymbolMap files which are used to resolve hidden symbols in dSYM files downloaded from iTunes Connect.  This requires the dsymutil tool to be available.  This should not be used when using the App Store Connect integration, the .bcsymbolmap files needed for the integration are uploaded without this option if they are found in the PATH searched for symbol files.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l info-plist -d 'Optional path to the Info.plist.{n}We will try to find this automatically if run from Xcode.  Providing this information will associate the debug symbols with a specific ITC application and build in Sentry.  Note that if you provide the plist explicitly it must already be processed.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l wait-for -d 'Wait for the server to fully process uploaded files, but at most for the given number of seconds. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l no-unwind -d 'Do not scan for stack unwinding information. Specify this flag for builds with disabled FPO, or when stackwalking occurs on the device. This usually excludes executables and dynamic libraries. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l no-debug -d 'Do not scan for debugging information. This will usually exclude debug companion files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l no-sources -d 'Do not scan for source information. This will usually exclude source bundle files. They might still be uploaded, if they contain additional processable information (see other flags).'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l require-all -d 'Errors if not all identifiers specified with --id could be found.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l derived-data -d 'Search for debug symbols in Xcode\'s derived data.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l no-zips -d 'Do not search in ZIP files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l no-upload -d 'Disable the actual upload.{n}This runs all steps for the processing but does not trigger the upload.  This is useful if you just want to verify the setup or skip the upload in tests.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l force-foreground -d 'Wait for the process to finish.{n}By default, the upload process will detach and continue in the background when triggered from Xcode.  When an error happens, a dialog is shown.  If this parameter is passed Xcode will wait for the process to finish before the build finishes and output will be shown in the Xcode build output.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l include-sources -d 'Include sources from the local file system and upload them as source bundles.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l wait -d 'Wait for the server to fully process uploaded files. Errors can only be displayed if --wait or --wait-for is specified, but this will significantly slow down the upload process.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l upload-symbol-maps
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l il2cpp-mapping -d 'Compute il2cpp line mappings and upload them along with sources.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-dsym" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -s o -l org -d 'The organization ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -s p -l project -d 'The project ID or slug.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l version -d 'Optionally associate the mapping files with a human readable version.{n}This helps you understand which ProGuard files go with which version of your app.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l version-code -d 'Optionally associate the mapping files with a version code.{n}This helps you understand which ProGuard files go with which version of your app.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l app-id -d 'Optionally associate the mapping files with an application ID.{n}If you have multiple apps in one sentry project, you can then easily tell them apart.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l platform -d 'Optionally defines the platform for the app association. [defaults to \'android\']' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l android-manifest -d 'Read version and version code from an Android manifest file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l write-properties -d 'Write the UUIDs for the processed mapping files into the given properties file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -s u -l uuid -d 'Explicitly override the UUID of the mapping file with another one.{n}This should be used with caution as it means that you can upload multiple mapping files if you don\'t take care.  This however can be useful if you have a build process in which you need to know the UUID of the proguard file before it was created.  If you upload a file with a forced UUID you can only upload a single proguard file.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l header -d 'Custom headers that should be attached to all requests{n}in key:value format.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l auth-token -d 'Use the given Sentry auth token.' -r
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l log-level -d 'Set the log output verbosity.' -r -f -a "{trace\t'',debug\t'',info\t'',warn\t'',error\t''}"
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l no-upload -d 'Disable the actual upload.{n}This runs all steps for the processing but does not trigger the upload.  This is useful if you just want to verify the mapping files and write the proguard UUIDs into a properties file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l require-one -d 'Requires at least one file to upload or the command will error.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l quiet -l silent -d 'Do not print any output while preserving correct exit code. This flag is currently implemented only for selected subcommands.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -l allow-failure -d 'Always return 0 exit code.'
complete -c sentry-cli -n "__fish_seen_subcommand_from upload-proguard" -s h -l help -d 'Print help'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "completions" -d 'Generate completions for the specified shell.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "bash-hook" -d 'Prints out a bash script that does error handling.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "debug-files" -d 'Locate, analyze or upload debug information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "deploys" -d 'Manage deployments for Sentry releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "events" -d 'Manage events on Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "files" -d 'Manage release artifacts.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "info" -d 'Print information about the configuration and verify authentication.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "issues" -d 'Manage issues in Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "login" -d 'Authenticate with the Sentry server.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "monitors" -d 'Manage cron monitors on Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "organizations" -d 'Manage organizations on Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "projects" -d 'Manage projects on Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "react-native" -d 'Upload build artifacts for react-native projects.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "releases" -d 'Manage releases on Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "repos" -d 'Manage repositories on Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "send-event" -d 'Send a manual event to Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "send-envelope" -d 'Send a stored envelope to Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "send-metric" -d 'Send a metric to Sentry.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "sourcemaps" -d 'Manage sourcemaps for Sentry releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "uninstall" -d 'Uninstall the sentry-cli executable.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "update" -d 'Update the sentry-cli executable.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "upload-dif" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "upload-dsym" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "upload-proguard" -d 'Upload ProGuard mapping files to a project.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from completions bash-hook debug-files deploys events files info issues login monitors organizations projects react-native releases repos send-event send-envelope send-metric sourcemaps uninstall update upload-dif upload-dsym upload-proguard help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c sentry-cli -n "__fish_seen_subcommand_from help debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload" -f -a "bundle-sources" -d 'Create a source bundle for a given debug information file'
complete -c sentry-cli -n "__fish_seen_subcommand_from help debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload" -f -a "check" -d 'Check the debug info file at a given path.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload" -f -a "bundle-jvm" -d 'Create a source bundle for the given JVM based source files (e.g. Java, Kotlin, ...)'
complete -c sentry-cli -n "__fish_seen_subcommand_from help debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload" -f -a "find" -d 'Locate debug information files for given debug identifiers.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload" -f -a "print-sources" -d 'Print source files linked by the given debug info file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help debug-files; and not __fish_seen_subcommand_from bundle-sources check bundle-jvm find print-sources upload" -f -a "upload" -d 'Upload debugging information files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help deploys; and not __fish_seen_subcommand_from list new" -f -a "list" -d 'List all deployments of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help deploys; and not __fish_seen_subcommand_from list new" -f -a "new" -d 'Creates a new release deployment.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help events; and not __fish_seen_subcommand_from list" -f -a "list" -d 'List all events in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "upload-sourcemaps" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "delete" -d 'Delete a release file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "list" -d 'List all release files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "upload" -d 'Upload files for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help issues; and not __fish_seen_subcommand_from list mute resolve unresolve" -f -a "list" -d 'List all issues in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help issues; and not __fish_seen_subcommand_from list mute resolve unresolve" -f -a "mute" -d 'Bulk mute all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help issues; and not __fish_seen_subcommand_from list mute resolve unresolve" -f -a "resolve" -d 'Bulk resolve all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help issues; and not __fish_seen_subcommand_from list mute resolve unresolve" -f -a "unresolve" -d 'Bulk unresolve all selected issues.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help monitors; and not __fish_seen_subcommand_from list run" -f -a "list" -d 'List all monitors for an organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help monitors; and not __fish_seen_subcommand_from list run" -f -a "run" -d 'Wraps a command'
complete -c sentry-cli -n "__fish_seen_subcommand_from help organizations; and not __fish_seen_subcommand_from list" -f -a "list" -d 'List all organizations available to the authenticated token.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help projects; and not __fish_seen_subcommand_from list" -f -a "list" -d 'List all projects for an organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help react-native; and not __fish_seen_subcommand_from gradle appcenter" -f -a "gradle" -d 'Upload react-native projects in a gradle build step.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help react-native; and not __fish_seen_subcommand_from gradle appcenter" -f -a "appcenter" -d 'Upload react-native projects for AppCenter.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "files" -d 'Manage release artifacts.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "deploys" -d 'Manage deployments for Sentry releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "archive" -d 'Archive a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "delete" -d 'Delete a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "finalize" -d 'Mark a release as finalized and released.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "info" -d 'Print information about a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "list" -d 'List the most recent releases.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "new" -d 'Create a new release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "propose-version" -d 'Propose a version name for a new release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "restore" -d 'Restore a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases; and not __fish_seen_subcommand_from files deploys archive delete finalize info list new propose-version restore set-commits" -f -a "set-commits" -d 'Set commits of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "upload-sourcemaps" -d 'Upload sourcemaps for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "delete" -d 'Delete a release file.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "list" -d 'List all release files.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases files; and not __fish_seen_subcommand_from upload-sourcemaps delete list upload" -f -a "upload" -d 'Upload files for a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases deploys; and not __fish_seen_subcommand_from list new" -f -a "list" -d 'List all deployments of a release.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help releases deploys; and not __fish_seen_subcommand_from list new" -f -a "new" -d 'Creates a new release deployment.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help repos; and not __fish_seen_subcommand_from list" -f -a "list" -d 'List all repositories in your organization.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help send-metric; and not __fish_seen_subcommand_from increment distribution gauge set" -f -a "increment" -d 'Increment a counter metric'
complete -c sentry-cli -n "__fish_seen_subcommand_from help send-metric; and not __fish_seen_subcommand_from increment distribution gauge set" -f -a "distribution" -d 'Update a distribution metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from help send-metric; and not __fish_seen_subcommand_from increment distribution gauge set" -f -a "gauge" -d 'Update a gauge metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from help send-metric; and not __fish_seen_subcommand_from increment distribution gauge set" -f -a "set" -d 'Update a set metric with the provided value'
complete -c sentry-cli -n "__fish_seen_subcommand_from help sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload" -f -a "explain" -d 'Explain why sourcemaps are not working for a given event.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload" -f -a "inject" -d 'Fixes up JavaScript source files and sourcemaps with debug ids.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload" -f -a "resolve" -d 'Resolve sourcemap for a given line/column position.'
complete -c sentry-cli -n "__fish_seen_subcommand_from help sourcemaps; and not __fish_seen_subcommand_from explain inject resolve upload" -f -a "upload" -d 'Upload sourcemaps for a release.'
