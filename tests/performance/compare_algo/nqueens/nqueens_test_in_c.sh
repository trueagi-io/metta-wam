#!/bin/bash

# Create a C program for the N-Queens problem
cat > nqueens.c <<'EOF'
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int is_safe(int board[], int row, int col) {
    for (int i = 0; i < col; i++) {
        if (board[i] == row || abs(board[i] - row) == abs(i - col)) {
            return 0;
        }
    }
    return 1;
}

void solve_nqueens(int board[], int col, int n, int *count) {
    if (col == n) {
        (*count)++;
        return;
    }
    for (int row = 0; row < n; row++) {
        if (is_safe(board, row, col)) {
            board[col] = row;
            solve_nqueens(board, col + 1, n, count);
        }
    }
}

int nqueens(int n) {
    int *board = (int *)malloc(n * sizeof(int));
    int count = 0;
    solve_nqueens(board, 0, n, &count);
    free(board);
    return count;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <board size>\n", argv[0]);
        return 1;
    }
    int n = atoi(argv[1]);
    clock_t start = clock();
    int solutions = nqueens(n);
    clock_t end = clock();
    double time_taken = (double)(end - start) / CLOCKS_PER_SEC;
    printf("N=%d: %d solutions found in %.6f seconds\n", n, solutions, time_taken);
    return 0;
}
EOF

# Compile the C program
gcc -O3 -o nqueens nqueens.c

# Benchmark for board sizes 4 through 10
echo "Benchmarking N-Queens (C implementation)"
for n in {4..15}; do
    echo "N=$n:"
    /usr/bin/time -f "Elapsed Time: %E" ./nqueens $n
    echo
done

# Clean up
rm -f nqueens nqueens.c

