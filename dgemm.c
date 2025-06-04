//#include <x86intrin.h> // Para intrínsecos AVX512, geralmente inclui immintrin.h
#include <time.h>      // Para clock_gettime, struct timespec
#include <stdio.h>     // Para printf, fprintf, NULL
#include <stdlib.h>    // Para atoi, srand, rand
// <immintrin.h> pode ser necessário explicitamente para _mm_malloc se não for puxado por x86intrin.h
// Em compiladores modernos como GCC/Clang, <x86intrin.h> é abrangente.
// Para MSVC, <intrin.h> e depois cabeçalhos específicos como <immintrin.h>.
// No Linux com GCC/Clang, <mm_malloc.h> também pode fornecer _mm_malloc.


void dgemm (int n, double* A, double* B, double* C)
{
    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
        {
            double cij = C[i+j*n]; /* cij = C[i][j] */
            for( int k = 0; k < n; k++ )
                cij += A[i+k*n] * B[k+j*n]; /* cij += A[i][k]*B[k][j] */
            C[i+j*n] = cij; /* C[i][j] = cij */
        }
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
    // if (n % (UNROLL * 8) != 0) {
    //     fprintf(stderr, "Erro: Para esta versão do dgemm, n (%d) deve ser um múltiplo de %d.\n", n, UNROLL * 8);
    //     return 1;
    // }

    // Alocar memória com alinhamento de 64 bytes para AVX512
    size_t num_elements = (size_t)n * n; // Usar size_t para evitar overflow em n*n
    double* A = (double*)malloc(num_elements * sizeof(double));
    double* B = (double*)malloc(num_elements * sizeof(double));
    double* C = (double*)malloc(num_elements * sizeof(double));

    if (A == NULL || B == NULL || C == NULL) {
        fprintf(stderr, "Erro: Falha na alocação de memória com _mm_malloc.\n");
        free(A); // _mm_free pode lidar com ponteiros NULL
        free(B);
        free(C);
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

    free(A);
    free(B);
    free(C);

    return 0;
}