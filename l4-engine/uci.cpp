
// Windows specific headers
#include <Windows.h>
#include <process.h>

enum {STATUS_PONDERING, STATUS_IDLE, STATUS_QUIT};

HANDLE thread_handle;
unsigned threadID;


int status = STATUS_IDLE;

// threading inspired by Maverick source code
unsigned __stdcall engine_loop(void* pArguments)
{
/*    FILE *f = fopen("file.txt", "w");
    if (f == NULL)
    {
        printf("Error opening file!\n");
        exit(1);
    }

    int time = 0;
    while (status != STATUS_QUIT) {
        fprintf(f, "time: %d\n", time);
        Sleep(CLOCKS_PER_SEC);
        time++;
    }

    fclose(f);
    printf("quitting thread\n");
    _endthreadex(0);*/
    return 0;
}

void create_thread()
{
    thread_handle = (HANDLE)_beginthreadex( NULL, 0, &engine_loop, NULL, 0, &threadID );
    SetThreadPriority(thread_handle, THREAD_PRIORITY_NORMAL); // needed for Fritz GUI! :-))
}