import json
import sys
import pathlib as pl

TEMPLATE = """# `std/{filename}`
Module documentation [here]({docfile}).
```som
{{{{#include {path}}}}}
```"""

context = None
project_dir = None
book = None

def log(obj):
	try: json.dump(obj, sys.stderr, indent=2)
	except TypeError: sys.stderr.write(f"{obj}")
	sys.stderr.write("\n")

def handle_source_file(path: pl.Path):
	basename = path.name.split(".")[0]
	content = TEMPLATE.format(
		filename = path.name,
		docfile = f"../stdlib/{basename}.md",
		path = path
	)

	number = len(book["sections"][-1]["Chapter"]["sub_items"])
	book["sections"][-1]["Chapter"]["sub_items"].append({
		"Chapter": {
			"name": f"HIDE",
			"content": content,
			"path": f"sources/{path.name}",
			"number": [100, number],
			"sub_items": [],
			"parent_names": ["sources"],
		}
	})

if __name__ == '__main__':
	if len(sys.argv) > 1:
		if sys.argv[1:3] == ["supports", "html"]: 
			sys.exit(0)

	context, book = json.load(sys.stdin)

	# # set output dir
	# build_dir = context["config"]["build"]["build-dir"]
	# output_at = context["config"]["preprocessor"]["gen-sources"]["output-dir"]
	# output_dir = pl.Path(context["root"]).joinpath(build_dir).joinpath(output_at)
	# output_dir.mkdir(parents=True, exist_ok=True)

	# set sources dir
	project_dir = pl.Path(context["root"]).parent
	sources_dir = project_dir.joinpath("stdlib").joinpath("modules")

	book["sections"].append({
		"Chapter": {
			"name": "HIDE",
			"content": "",
			"number": [100],
			"sub_items": [],
			"parent_names": []
		}
	})
	
	for file in sources_dir.iterdir():
		handle_source_file(file)

	log(book)

	print(json.dumps(book))