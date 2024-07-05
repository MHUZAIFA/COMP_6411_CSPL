import socket
import os
import re

def clear_screen():
    os.system('clear' if os.name == 'posix' else 'cls')

def print_menu():
    clear_screen()
    print("Customer Management Menu")
    print("1. Find customer")
    print("2. Add customer")
    print("3. Delete customer")
    print("4. Update customer age")
    print("5. Update customer address")
    print("6. Update customer phone")
    print("7. Print report")
    print("8. Exit")
    print("\nSelect: ", end='')

def get_input(prompt, validation_func=None, error_message="Invalid input, please try again."):
    while True:
        input_value = input(prompt).strip()
        if not validation_func or validation_func(input_value):
            return input_value
        print(error_message)

def validate_name(name):
    return name.isalpha()

def validate_age(age):
    return age == "" or age.isdigit() and 1 <= int(age) <= 120

def validate_address(address):
    return address == "" or all(c.isalnum() or c in ' .-' for c in address)

def validate_phone(phone):
    if phone == "":
        return True
    # Remove spaces and dashes
    digitsCount = len(phone.replace(' ', '').replace('-', ''))
    
    # Check for 7-digit phone number
    if digitsCount == 7:
        pattern = r'^\d{3}-\d{4}$'
        return re.match(pattern, phone)
    
    # Check for 10-digit phone number
    elif digitsCount == 10:
        pattern = r'^\d{3} \d{3}-\d{4}$'
        return re.match(pattern, phone)
    
    else:
        return False

def communicate_with_server(message):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.connect(('localhost', 9999))
        sock.sendall(message.encode())
        response = sock.recv(1024).decode()
    return response

def main():
    while True:
        print_menu()
        choice = get_input("")
        if choice == '1':
            name = get_input("Enter customer name: ", validate_name)
            response = communicate_with_server(f"find|{name}")
        elif choice == '2':
            name = get_input("Enter name: ", validate_name)
            age = get_input("Enter age (leave blank if not known): ", validate_age, "Invalid age, please enter a valid age between 1 and 120.")
            address = get_input(
                "Enter address (letters, numbers, spaces, dots, and dashes allowed): ",
                validate_address,
                "Invalid address, please use only letters, numbers, spaces, dots, and dashes."
            )
            phone = get_input(
                "Enter phone (format xxx-xxxx or xxx xxx-xxxx): ",
                validate_phone,
                "Invalid phone number. Enter as xxx-xxxx or xxx xxx-xxxx."
            )
            response = communicate_with_server(f"add|{name}|{age}|{address}|{phone}")
        elif choice == '3':
            name = get_input("Enter customer name to delete: ", validate_name)
            response = communicate_with_server(f"delete|{name}")
        elif choice == '4':
            name = get_input("Enter customer name: ", validate_name)
            new_age = get_input("Enter new age: ", validate_age, "Invalid age, please enter a valid age between 1 and 120.")
            response = communicate_with_server(f"update|{name}|age|{new_age}")
        elif choice == '5':
            name = get_input("Enter customer name: ", validate_name)
            new_address = get_input(
                "Enter new address (letters, numbers, spaces, dots, and dashes allowed): ",
                validate_address,
                "Invalid address, please use only letters, numbers, spaces, dots, and dashes."
            )
            response = communicate_with_server(f"update|{name}|address|{new_address}")
        elif choice == '6':
            name = get_input("Enter customer name: ", validate_name)
            new_phone = get_input(
                "Enter new phone number (format xxx-xxxx or xxx xxx-xxxx): ",
                validate_phone,
                "Invalid phone number. Enter as xxx-xxxx or xxx xxx-xxxx."
            )
            response = communicate_with_server(f"update|{name}|phone|{new_phone}")
        elif choice == '7':
            response = communicate_with_server("report|")
        elif choice == '8':
            print("Good bye")
            break
        else:
            response = "Invalid option"

        print(response)
        input("Press any key to continue...")

if __name__ == '__main__':
    main()
