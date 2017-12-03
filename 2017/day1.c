#include "stdio.h"
#include "stdlib.h"
#include "string.h"

struct list
{
    int value;
    struct list *next;
};

int sum_next(struct list *head, int list_size)
{
    int sum = 0;
    struct list *curr_elem = head;
    struct list *next_elem = head->next;
    while (list_size > 0) {
        if (curr_elem->value == next_elem->value)
            sum += curr_elem->value;
        curr_elem = curr_elem->next;
        next_elem = next_elem->next;
        list_size--;
    }
    return sum;
}

int sum_half(struct list *head, int list_size)
{
    int sum = 0;
    struct list *curr_elem = head;
    struct list *half_elem = head;

    int j;
    for (j = 0; j < list_size / 2; j++)
        half_elem = half_elem->next;

    while (list_size > 0) {
        if (curr_elem->value == half_elem->value)
            sum += curr_elem->value;
        curr_elem = curr_elem->next;
        half_elem = half_elem->next;
        list_size--;
    }
    return sum;
}

struct list* str_to_list(char str[], int *size)
{
    int i = 0;
    struct list *prev_elem;
    struct list *first_elem;
    while (str[i] != '\0') {
        struct list *elem = malloc(sizeof(struct list));
        elem->value = str[i] - '0';
        if (i != 0) prev_elem->next = elem;
        else first_elem = elem;
        prev_elem = elem;
        i++;
    }
    *size = i;
    prev_elem->next = first_elem;
    return first_elem;
}

int main(int argc, char **argv)
{
    int next = 0;
    if (argc > 1) {
        if (strcmp(argv[1], "next") == 0) {
            next = 1;
        } else if (strcmp(argv[1], "half") == 0) {
            next = 0;
        } else {
            puts("Please choose method a method!");
            printf("%s next or %s half\n", argv[0], argv[0]);
            return 1;
        }
    } else {
        puts("Please choose method a method!");
        printf("%s next or %s half\n", argv[0], argv[0]);
        return 1;
    }

    char input[4096];
    scanf("%4096s", input);
    int list_size;
    struct list *head = str_to_list(input, &list_size);

    int sum = 0;
    if (next) {
        sum = sum_next(head, list_size);
    } else {
        sum = sum_half(head, list_size);
    }

    printf("%d\n", sum);
    return 0;
}
