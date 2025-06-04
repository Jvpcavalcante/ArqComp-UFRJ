#include <x86intrin.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#define UNROLL (4)
#define BLOCKSIZE 32
void do_block (int n, int si, int sj, int sk,
               double *A, double *B, double *C)
{
    for ( int i = si; i < si+BLOCKSIZE; i+=UNROLL*8 )
        for ( int j = sj; j < sj+BLOCKSIZE; j++ ) {
            __m512d c[UNROLL];
            for (int r=0;r<UNROLL;r++)
                c[r] = _mm512_load_pd(C+i+r*8+j*n); // [ UNROLL];

            for ( int k = sk; k < sk+BLOCKSIZE; k++ )
            {
                __m512d bb = _mm512_broadcastsd_pd(_mm_load_sd(B+j*n+k));
                for (int r=0;r<UNROLL;r++)
                    c[r] = _mm512_fmadd_pd(_mm512_load_pd(A+n*k+r*8+i), bb, c[r]);
            }

            for (int r=0;r<UNROLL;r++)
                _mm512_store_pd(C+i+r*8+j*n, c[r]);
        }
}

void dgemm (int n, double* A, double* B, double* C)
{
#pragma omp parallel for
    for ( int sj = 0; sj < n; sj += BLOCKSIZE )
        for ( int si = 0; si < n; si += BLOCKSIZE )
            for ( int sk = 0; sk < n; sk += BLOCKSIZE )
                do_block(n, si, sj, sk, A, B, C);
}

// Modificado para também inicializar C com zeros
void generate_matrices(int n, double* A, double* B, double* C_matrix_to_initialize){
    srand(1); // Usar um seed fixo para reprodutibilidade durante o debug
    for (int i = 0; i < n*n; ++i){
        A[i] = (double)(rand() % 100) / 10.0; // Valores entre 0.0 e 9.9
        B[i] = (double)(rand() % 100) / 10.0;
        if (C_matrix_to_initialize != NULL) {
            C_matrix_to_initialize[i] = 0.0; // Inicializa C com zeros
        }
    }
}

int main(int argc, char* argv[]){
    if (argc < 2) {
        fprintf(stderr, "Uso: %s <tamanho_da_matriz_n>\n", argv[0]);
        return 1;
    }

    int n = atoi(argv[1]);

    if (n <= 0) {
        fprintf(stderr, "Erro: O tamanho da matriz n deve ser um inteiro positivo.\n");
        return 1;
    }

    // Validação crucial para esta implementação de dgemm
    if (n % (UNROLL * 8) != 0) {
        fprintf(stderr, "Erro: Para esta versão do dgemm, n (%d) deve ser um múltiplo de %d.\n", n, UNROLL * 8);
        return 1;
    }

    // Alocar memória com alinhamento de 64 bytes para AVX512
    size_t num_elements = (size_t)n * n; // Usar size_t para evitar overflow em n*n
    double* A = (double*)_mm_malloc(num_elements * sizeof(double), 64);
    double* B = (double*)_mm_malloc(num_elements * sizeof(double), 64);
    double* C = (double*)_mm_malloc(num_elements * sizeof(double), 64);

    if (A == NULL || B == NULL || C == NULL) {
        fprintf(stderr, "Erro: Falha na alocação de memória com _mm_malloc.\n");
        _mm_free(A);
        _mm_free(B);
        _mm_free(C);
        return 1;
    }

    // Inicializa matrizes A, B, e C (C será zerada)
    generate_matrices(n, A, B, C);

    struct timespec start_time, end_time; // Renomeado para evitar conflitos
    clock_gettime(CLOCK_MONOTONIC, &start_time);

    dgemm(n, A, B, C);

    clock_gettime(CLOCK_MONOTONIC, &end_time);

    double time_taken = (end_time.tv_sec - start_time.tv_sec) +
                        (end_time.tv_nsec - start_time.tv_nsec) / 1e9;

    printf("N = %d\n", n);
    printf("Tempo gasto: %f segundos\n", time_taken);

    // Opcional: verificar alguns valores de C
    // if (n > 0 && num_elements > 0) {
    //     printf("C[0] = %f\n", C[0]);
    //     printf("C[ultima_pos] = %f\n", C[num_elements - 1]);
    // }

    _mm_free(A);
    _mm_free(B);
    _mm_free(C);

    return 0;
}