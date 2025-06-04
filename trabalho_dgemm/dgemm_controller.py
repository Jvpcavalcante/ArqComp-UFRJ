import subprocess
import sys
import os

def compile_dgemm(other_compile_flags):  # Renomeei 'flag' para 'other_compile_flags' para maior clareza
    """
    Compila o programa dgemm.c usando gcc, incluindo a flag -mavx512f
    para suporte a AVX512 (arquitetura x86).
    """
    try:
        compile_cmd_list = ['gcc']

        # Adiciona as 'other_compile_flags' se forem fornecidas
        if other_compile_flags:
            if isinstance(other_compile_flags, str):
                # Se 'other_compile_flags' for uma string, divide-a
                # para tratar flags múltiplas separadas por espaço (ex: "-O3 -march=native")
                compile_cmd_list.extend(other_compile_flags.split())
            elif isinstance(other_compile_flags, list):
                # Se já for uma lista, apenas a estende
                compile_cmd_list.extend(other_compile_flags)

        # Adiciona a flag -mavx512f
        #compile_cmd_list.append('-mavx512f')

        # Adiciona o arquivo fonte e o nome do arquivo de saída
        compile_cmd_list.extend(['dgemm.c', '-o', 'dgemm'])
        
        # Filtra quaisquer strings vazias que possam ter surgido (ex: de um split())
        compile_cmd_list = [cmd_part for cmd_part in compile_cmd_list if cmd_part]

        print(f"Executando comando de compilação: {' '.join(compile_cmd_list)}") # Adicionado para depuração
        result = subprocess.run(compile_cmd_list, capture_output=True, text=True, check=False) # check=False para tratar o returncode manualmente

        if result.returncode != 0:
            print("Falha na compilação:")
            print(f"Comando: {' '.join(compile_cmd_list)}") # Imprime o comando em caso de falha
            print(result.stderr)
            return False

        print("Compilação bem-sucedida!")
        return True
    except Exception as e:
        print(f"Erro durante a compilação: {str(e)}")
        return False

def run_dgemm(matrix_size):
    """Run the compiled dgemm program with the specified matrix size."""
    try:
        # Run the program with the matrix size as argument
        run_cmd = ['./dgemm', str(matrix_size)]
        result = subprocess.run(run_cmd, capture_output=True, text=True)

        if result.returncode != 0:
            print("Execution failed:")
            print(result.stderr)
            return False

        print(result.stdout)
        return True

    except Exception as e:
        print(f"Error during execution: {str(e)}")
        return False

def main():
    matrix_sizes = [1024, 2048, 4096, 8192]
    flags = ["-O0","-O1","-O2","-O3"]

    for matrix_size in matrix_sizes:
        for flag in flags:
            if not compile_dgemm(flag):
                sys.exit(1)

            for i in range(1):
                print(f"Running dgemm with matrix size {matrix_size} and flag {flag} {i+1} times")
                if not run_dgemm(matrix_size):
                    sys.exit(1)

if __name__ == "__main__":
    main()

#python dgemm_controller.py >> log.txt 2>&1 &