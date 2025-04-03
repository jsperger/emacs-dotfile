text-mode

(commit-basic-template "<type>[optional scope]: <description>")
(cbbuild "build: " r)
(cbchore "chore: " r)
(cbdocs "docs: " r)
(cbfeat "feat: " r)
(cbfix "fix: " r)
(cbperf "perf: " r)
(cbrefactor "refactor: " r)
(cbrevert "revert: " r)
(cbstyle "style: " r)
(cbtest "test: " r)

;; Commit emoji template
(cebuild "🏗️: " r)
(cechore "🧹: " r)
(cedocs "📚: " r)
(cetoolbox "🧰: " r)
(cetoggle "🪃: toggle enable/disable" r)
(commit-full-template "<type>[optional scope]: <description>" n>
											p
											n>
											"[optional footer(s)]")
