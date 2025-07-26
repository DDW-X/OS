// placeholder
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/wait.h>

#define DROPPER_PATH "build/user/dropper"

int main() {
    pid_t pid = fork();
    if (pid == 0) {
        // Child process
        execl(DROPPER_PATH, DROPPER_PATH, NULL);
        perror("execl");
        exit(1);
    } else if (pid > 0) {
        // Parent process
        int status;
        waitpid(pid, &status, 0);
        
        if (WIFEXITED(status)) {
            printf("Dropper exited with status: %d\n", WEXITSTATUS(status));
            if (WEXITSTATUS(status) == 0) {
                printf("Module activation successful\n");
                return 0;
            }
        }
    }
    
    printf("Test failed\n");
    return 1;
}
