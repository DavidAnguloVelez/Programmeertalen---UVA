import random
import copy
import time
import itertools as iter


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

    def __add__(self, other):
        new_items = Items()
        new_item_list = [i for i in self.item_list if i in other.item_list]
        new_items.add_item_list(new_item_list)
        return new_items

    def __sub__(self, other):
        new_items = Items()
        new_item_list = [i for i in self.item_list if i not in other.item_list]
        new_items.add_item_list(new_item_list)
        return new_items

    def add_item(self, item: Item) -> None:
        self.item_list.append(item)
        return self

    def add_item_list(self, item_list: list) -> None:
        for item in item_list:
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

    def get_items(self) -> Items:
        return self.items

    def get_resources(self) -> Resources:
        return self.resources

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
            return False
        elif (self.get_weight() + item.get_weight() >
                self.resources.get_weight()):
            return False
        elif item in self.get_item_list():
            return False

        self.items.add_item(item)
        self.points += item.points
        return True

    # Removes an item from the knapsack
    def remove_item(self, item: Item) -> None:
        if item in self.items.item_list:
            self.points -= item.get_points()
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

    def empty(self) -> None:
        while len(self) > 0:
            self.pop()

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
        self.best_knapsack = knap

        for _ in range(self.tries):
            k = Knapsack(Items(), knap.resources)
            random.shuffle(items.get_item_list())

            for item in items.item_list:
                check_added = k.add_item(item)
                if not check_added:
                    break

            if k.get_points() > self.best_knapsack.get_points():
                self.best_knapsack = copy.deepcopy(k)

    # Returns the best knapsack
    def get_best_knapsack(self) -> Knapsack:
        if self.best_knapsack is not None:
            return self.best_knapsack
        else:
            return False


class Solver_Optimal_Recursive(Solver):
    def __init__(self) -> None:
        Solver.__init__(self)

    def solve(self, knap: Knapsack, items: Items) -> None:
        # Calls the recursive function
        self.recursion(knap, items, 0)
        # self.better_recursion(knap, items.get_item_list(),
        #                       items.get_item_list()[-1:])

    # def better_recursion(self, knap: Knapsack, items_left: list,
    #                      items_added: list) -> None:
    #     # Checks if no best_knapsack was assigned yet.
    #     if self.best_knapsack is None:
    #         self.best_knapsack = copy.deepcopy(knap)

    #     if len(items_left) == 0:
    #         return
    #     else:
    #         # time.sleep(1)
    #         first_element = items_left[0]
    #         for item in items_added:
    #             knap.add_item(item)
    #         # If a better knapsack is found, assign it as the best knapsack.
    #         if knap.get_points() > self.best_knapsack.get_points():
    #             self.best_knapsack = copy.deepcopy(knap)

    #         print(knap)
    #         # print("Items left: ", items_left)
    #         # print("Items added: ", items_added)

    #         if knap.item_fits(first_element):
    #             added_copy1 = items_added
    #             added_copy1.append(first_element)
    #             # print("RECURSION ADDED")
    #             self.better_recursion(knap, items_left[1:],
    #                                   added_copy1)

    #         # added_copy2 = copy.deepcopy(items_added)
    #         # print("RECURSION  NOT  ADDED")
    #         self.better_recursion(knap,
    #                               items_left[1:],
    #                               items_added)

    def recursion(self, knap: Knapsack, items: Items, index: int) -> None:
        # Checks if no best_knapsack was assigned yet.
        if self.best_knapsack is None:
            self.best_knapsack = copy.deepcopy(knap)

        # If a better knapsack is found, assign it as the best knapsack.
        if knap.get_points() > self.best_knapsack.get_points():
            self.best_knapsack = copy.deepcopy(knap)

        # When done looping through all items, stop recursion (base case).
        if index >= len(items):
            return
        else:
            # print(knap)
            if knap.item_fits(items.get_item_list()[index]):
                knap.add_item(items.get_item_list()[index])
                self.recursion(knap, items, index + 1)

            knap.remove_item(items.get_item_list()[index])
            self.recursion(knap, items, index + 1)
            # # If the item still fits in the backpack, make a recursive
            # # call with the current knapsack plus the item added.
            # if knap.item_fits(items.get_item_list()[index]):
            #     k1
            #     k1.add_item(items.get_item_list()[index])
            #     self.recursion(k1, items, index + 1)

            # # Make a recursive call, with no modification to the current
            # # knapsack, but make it check with other items (index + 1).
            # self.recursion(knap, items, index + 1)

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Optimal_Iterative_Deepcopy(Solver):
    def __init__(self) -> None:
        Solver.__init__(self)

    def solve(self, knap: Knapsack, items: Items) -> None:
        # When no best_knapsack was given yet:
        self.best_knapsack = knap

        stack = [(copy.deepcopy(knap), 0)]

        while len(stack) != 0:
            knap, index = stack.pop()

            if knap.get_points() > self.best_knapsack.get_points():
                self.best_knapsack = copy.deepcopy(knap)

            if index < len(items):
                item = items.get_item_list()[index]
                if knap.item_fits(item):
                    k2: Knapsack = copy.deepcopy(knap)
                    k2.add_item(item)
                    stack.append((k2, index + 1))

                stack.append((knap, index + 1))

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Optimal_Iterative(Solver):
    def __init__(self) -> None:
        Solver.__init__(self)

    # Solves the given knapsack with the given items
    def solve(self, knap: Knapsack, items: Items) -> None:
        self.best_knapsack = knap

        for item_combi in iter.combinations(items.get_item_list(), len(items)):
            # print("LOOPING WOOHOO")
            for item in item_combi:
                # print("ADDINGG")
                check_added = knap.add_item(item)
                if not check_added:
                    # print("LOOPING DIED :(")
                    break

            if knap.get_points() > self.best_knapsack.get_points():
                self.best_knapsack = copy.deepcopy(knap)

            knap.empty()

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Random_Improved(Solver):
    def __init__(self, tries: int) -> None:
        Solver.__init__(self)
        self.tries = tries

    def solve(self, knap: Knapsack, items: Items) -> None:
        self.best_knapsack = knap
        return
        # random.shuffle(items.get_item_list())
        # length = len(items)

        # for i in range(self.tries):
        #     index = random.randint(0, length)

        #     points_before = items.get_total_points()
        #     popped = items.get_item_list().pop(index)

        #     points_after = idk
        #     if points_before > points_after:

        # self.best_knapsack = knap

    def get_best_knapsack(self):
        return self.best_knapsack


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
    # itm = Item("name", 10, Resources(10, 10))
    # itms = Items()
    # knap = Knapsack(itms, Resources(1000, 100))
    # knap.add_item(itm)
    # print(knap)
    # knap.remove_item(itm)
    # print(knap)
    # return
    solver_random = Solver_Random(1000)
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_recursive = Solver_Optimal_Recursive()
    solver_random_improved = Solver_Random_Improved(5000)

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file +
          ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_iterative, knapsack_file +
          ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_optimal_recursive, knapsack_file +
          ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_random_improved, knapsack_file +
          ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file +
          ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_iterative.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file +
          "_solution_random.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")


if __name__ == "__main__":  # keep this at the bottom of the file
    main()
