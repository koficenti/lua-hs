CC = clang

define app

#include <stdio.h>

int main() {
	printf("hello world!");
	return 0;
}

endef
export app

app:
	make app.c
	@printf "\n--RUNNING APP\n\n"
	@app.exe
	@printf "\n\n--FINISHED--\n\n"
	make clean
app.c:
	echo "$$app" > app.c
	${CC} app.c -o "app.exe"
clean:
	rm -f app.c app.exe app.pdb