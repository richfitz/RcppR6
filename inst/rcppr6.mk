## Avoid a little repetition by putting a few things here.
RCPPR6_PATH ?= extra

cog:
	cog.py -I${RCPPR6_PATH} -r @${RCPPR6_PATH}/generation_list.txt

uncog:
	cog.py -I${RCPPR6_PATH} -r -x @${RCPPR6_PATH}/generation_list.txt
