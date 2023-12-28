import random
import sys

# Use this script from the command line to generate test data for the genetic algorithm.
# The script takes one argument, the number of tasks to generate.
# Example: python ./GA_testDataGenerator.py 1000

def generate_task_lines(num_tasks):
    letters = ['A', 'B', 'C', 'D']
    numbers = [1, 2, 3]
    cel_values = range(1, 10)
    categories = ['PickUpAndDelivery', 'Surveillance']

    lines = ["testTaskData(["]
    lines.append("  robot('robot1',origin('B',2,cel(1,9))),")

    for i in range(1, num_tasks + 1):
        origin_letter = random.choice(letters)
        origin_number = random.choice(numbers)
        origin_cel = random.choice(cel_values)

        destination_letter = random.choice(letters)
        destination_number = random.choice(numbers)
        destination_cel = random.choice(cel_values)

        category = random.choice(categories)

        task_line = f"  task('robot1','task{str(i).zfill(4)}',origin('{origin_letter}',{origin_number},cel({origin_cel},9)),destination('{destination_letter}',{destination_number},cel({destination_cel},8)),'{category}'),"
        lines.append(task_line)

    lines.append("\n\n\n\n\n\n\n\n\n\n\n\n\n\n  endOfTasks()")
    lines.append("]).")

    return lines

def main():
    if len(sys.argv) != 2:
        print("Usage: python script_name.py <number_of_tasks>")
        sys.exit(1)

    num_tasks = int(sys.argv[1])
    task_lines = generate_task_lines(num_tasks)

    with open("src/geneticTestData.pl", "w") as file:
        for line in task_lines:
            file.write(line + "\n")

if __name__ == "__main__":
    main()
