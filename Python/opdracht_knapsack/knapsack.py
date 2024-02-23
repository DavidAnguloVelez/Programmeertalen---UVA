import random
import time
import copy


class Resources:
    def __init__(self, weight: int, volume: int) -> None:
        self.weight = weight
        self.volume = volume

    def __repr__(self) -> str:
        return f"{self.weight} {self.volume}"

    def get_weight(self) -> int:
        return self.weight

    def get_volume(self) -> int:
        return self.volume

    def set_weight(self, weight: int) -> None:
        self.weight = weight

    def set_volume(self, volume: int) -> None:
        self.volume = volume


class Item:
    def __init__(self, name: str, points: int, resources: Resources) -> None:
        self.name = name
        self.points = points
        self.resources = resources

    def __repr__(self) -> str:
        return (f"{self.name}:{self.points}:{self.get_weight()}:{self.get_volume()}")

    def get_name(self) -> str:
        return self.name

    def get_points(self) -> int:
        return self.points

    def get_weight(self) -> int:
        return self.resources.get_weight()

    def get_volume(self) -> int:
        return self.resources.get_volume()

    def set_name(self, name: str) -> None:
        self.name = name

    def set_points(self, points: int) -> None:
        self.points = points

    def set_weight(self, weight: int) -> None:
        self.resources.set_weight(weight)

    def set_volume(self, volume: int) -> None:
        self.resources.set_volume(volume)


class Items:
    def __init__(self) -> None:
        self.item_list = []

    def __repr__(self) -> str:
        item_str = ""

        for item in self.item_list:
            item_str += item.name
            item_str += "\n"

        return item_str

    def __len__(self) -> list[Item]:
        return len(self.item_list)

    def add_item(self, item: Item) -> None:
        self.item_list.append(item)

    # Removes given item from items.item_list
    def remove_item(self, item: Item) -> None:
        """
        >>> its = Items()
        >>> i1 = Item("item1",70, 0, 0)
        >>> i2 = Item("item2", 2, 0, 0)
        >>> its.add_item(i1)
        >>> its.add_item(i2)
        >>> s = str(its)
        >>> "item2" in s
        True
        >>> its.remove_item(i2)
        >>> s = str(its)
        >>> "item2" in s
        False
        """
        self.item_list.remove(item)

    def pop(self) -> Item:
        return self.item_list.pop()

    def get_item_list(self) -> list[Item]:
        return self.item_list

    #  Gets the total points of all different items in the items.item_list
    def get_total_points(self) -> int:
        """
        >>> its = Items()
        >>> i1 = Item("item1",70, 0, 0)
        >>> i2 = Item("item2", 2, 0, 0)
        >>> its.add_item(i1)
        >>> its.add_item(i2)
        >>> its.get_total_points()
        72
        """

        return sum(item.get_points() for item in self.item_list)

    #  Gets the total weight of all different items in the items.item_list
    def get_total_weight(self) -> int:
        """
        >>> its = Items()
        >>> i1 = Item("item1", 0, 13, 0)
        >>> i2 = Item("item2", 0, 9, 0)
        >>> its.add_item(i1)
        >>> its.add_item(i2)
        >>> its.get_total_weight()
        22
        """
        return sum(item.get_weight() for item in self.item_list)

    #  Gets the total volume of all different items in the items.item_list
    def get_total_volume(self) -> int:
        """
        >>> its = Items()
        >>> i1 = Item("item1", 0, 0, 43)
        >>> i2 = Item("item2", 0, 0, 9)
        >>> its.add_item(i1)
        >>> its.add_item(i2)
        >>> its.get_total_volume()
        52
        """

        return sum(item.get_volume() for item in self.item_list)

    def find_item_index(self, item) -> int:
        return self.item_list.index(item)


class Knapsack:
    def __init__(self, items: Items, resources: Resources):
        self.items: Items = items
        self.resources: Resources = resources
        self.points: int = items.get_total_points()

    def __repr__(self) -> str:
        if (len(self.items) == 0):
            return "Knapsack is empty."
        else:
            return f"points:{self.get_points()}\n{self.items}"

    def __len__(self) -> int:
        return len(self.items)

    # Returns the current points stored in the knapsack
    def get_points(self) -> int:
        return self.points

    # Returns the current volume stored in the knapsack
    def get_volume(self) -> int:
        return self.items.get_total_volume()

    # Returns the current weight stored in the knapsack
    def get_weight(self) -> int:
        return self.items.get_total_weight()

    def get_max_weight(self) -> int:
        return self.resources.get_weight()

    def get_max_volume(self) -> int:
        return self.resources.get_volume()

    def get_item_list(self) -> list[Item]:
        return self.items.get_item_list()

    def find_item(self, item: Item) -> bool:
        return item in self.get_item_list()

    # Adds an item into the knapsack
    def add_item(self, item: Item) -> bool:
        """
        >>> it1 = Item("item1", 1, 1, 1)
        >>> its = Items()
        >>> knap = Knapsack(its, Resources(50, 50))
        >>> knap.add_item(it1)
        True
        >>> it1 in knap.items.item_list
        True
        >>> it2 = Item("item2", 1000, 1000, 1000)
        >>> knap.add_item(it2)
        False
        """
        if self.get_volume() + item.get_volume() > self.resources.get_volume():
            print("Volume too much")
            return False
        elif (self.get_weight() + item.get_weight() >
                self.resources.get_weight()):
            print("Weight too much")
            return False
        elif item in self.get_item_list():
            print("Item found lmao")
            return False

        self.items.add_item(item)
        self.points += item.points
        return True

    # Removes an item from the knapsack
    def remove_item(self, item: Item) -> None:
        if item in self.items.item_list:
            self.points += item.points
            self.items.remove_item(item)

    def item_fits(self, item) -> bool:
        if self.get_volume() + item.get_volume() > self.resources.get_volume():
            return False
        elif (self.get_weight() + item.get_weight() >
                self.resources.get_weight()):
            return False
        else:
            return True

    def pop(self) -> Item:
        return self.items.pop()

    # Saves the knapsack points and items inside to file
    def save(self, file: str) -> None:
        with open(file, "w") as f:
            f.write(f"{self}")


class Solver:
    def __init__(self):
        self.best_knapsack: Knapsack = None


class Solver_Random(Solver):
    def __init__(self, tries: int) -> None:
        super().__init__
        self.tries = tries

    # Solves the given knapsack with the given items
    def solve(self, knap: Knapsack, items: Items) -> None:
        max_points = 0
        self.best_knapsack = knap

        for i in range(self.tries):
            random.shuffle(items.item_list)

            for item in items.item_list:
                check_added = knap.add_item(item)
                if not check_added:
                    break

            if knap.get_points() > max_points:
                max_points = knap.get_points()
                self.best_knapsack = knap

    # Returns the best knapsack
    def get_best_knapsack(self) -> Knapsack:
        if self.best_knapsack is not None:
            return self.best_knapsack
        else:
            return False


class Solver_Optimal_Iterative(Solver):
    def __init__(self) -> None:
        Solver.__init__(self)

    # Solves the given knapsack with the given items
    def solve(self, knap: Knapsack, items: Items) -> None:
        pass

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Optimal_Recursive(Solver):
    def __init__(self) -> None:
        Solver.__init__(self)

    def compare_knaps(self, k1: Knapsack, k2: Knapsack) -> Knapsack:
        if k1.get_points() > k2.get_points():
            return k1
        else:
            return k2

    def solve(self, knap: Knapsack, items: Items) -> None:
        self.recursion(knap, items, 0)

    def recursion(self, knap: Knapsack, items: Items, index: int):
        if self.best_knapsack is None:
            self.best_knapsack = copy.deepcopy(knap)

        if knap.get_points() > self.best_knapsack.get_points():
            self.best_knapsack = copy.deepcopy(knap)

        if index >= len(items):
            return
        else:
            if knap.item_fits(items.get_item_list()[index]):
                k1: Knapsack = copy.deepcopy(knap)
                k1.add_item(items.get_item_list()[index])
                self.recursion(k1, items, index + 1)

            k2: Knapsack = knap
            self.recursion(k2, items, index + 1)

    # def recursion(self, knap: Knapsack, items: Items) -> Knapsack:
    #     # time.sleep(0.5)
    #     # Base case
    #     if len(items) == 0:
    #         return knap
    #     else:
    #         item = items.pop()
    #         if not knap.item_fits(item):
    #             return knap
    #         else:
    #             k1: Knapsack = copy.deepcopy(knap)
    #             k2: Knapsack = copy.deepcopy(knap)
    #             k2.add_item(item)
    #             # print("k1: ", k1, "\nk2: ", k2)
    #             return self.compare_knaps(
    #                 self.recursion(k1, items),
    #                 self.recursion(k2, items))

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Optimal_Iterative_Deepcopy(Solver):
    def __init__(self) -> None:
        Solver.__init__(self)


class Solver_Random_Improved(Solver):
    def __init__(self) -> None:
        Solver.__init__(self)


def load_knapsack(file: str) -> tuple[Knapsack, Items]:
    with open(file, "r") as f:
        next(f)

        # Initiliase
        knap_line = f.readline()
        knap_str_arr = knap_line.split(",")

        weight = int(knap_str_arr[2])
        volume = int(knap_str_arr[3])
        res = Resources(weight, volume)
        items = Items()
        empty_items = Items()

        knap = Knapsack(empty_items, res)

        for line in f:
            line_arr = line.split(",")

            i_name = line_arr[0]
            i_points = int(line_arr[1])
            i_weight = int(line_arr[2])
            i_volume = int(line_arr[3])

            item = Item(i_name, i_points, Resources(i_weight, i_volume))
            items.add_item(item)

    return knap, items


def solve(solver: Solver, knapsack_file: str, solution_file: str) -> None:
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    if knapsack is not None:
        print(f"saving solution with {knapsack.get_points()} points to '{solution_file}'")
        knapsack.save(solution_file)
    else:
        print("No best knapsack was defined.")


def main() -> None:
    # itms = Items()
    # knap = Knapsack(itms, Resources(10, 10))
    # itm = Item("item1", 1000, Resources(10, 10))
    # knap.add_item(itm)
    # print(knap.get_points())

    # return
    # solver_random = Solver_Random(1000)
    # solver_optimal_iterative = Solver_Optimal_Iterative()
    # solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_recursive = Solver_Optimal_Recursive()
    # solver_random_improved = Solver_Random_Improved(5000)

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    # solve(solver_random, knapsack_file +
        #   ".csv", knapsack_file + "_solution_random.csv")
    # solve(solver_optimal_iterative, knapsack_file +
            # ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_optimal_recursive, knapsack_file +
          ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    # solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
    #       knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    # solve(solver_random_improved, knapsack_file +
            # ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    # solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    # solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    # solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
    #       knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    # knapsack_file = "knapsack_large"
    # print("=== solving:", knapsack_file)
    # solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    # solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")


if __name__ == "__main__":  # keep this at the bottom of the file
    main()
