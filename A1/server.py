import socketserver
import re

# Data loading and validation functions
def load_database(filename):
    customers = {}
    with open(filename, 'r') as file:
        for line in file:
            original_line = line.strip()
            parts = [p.strip() for p in line.split('|')]
            if not parts[0]:
                print(f"Record skipped [missing field(s)]: {original_line}")
                continue
            if len(parts) < 4:
                print(f"Record skipped [missing field(s)]: {original_line}")
                continue
            if parts[0].lower() in customers:
                print(f"Record skipped [duplicate name]: {original_line}")
                continue
            if not validate_customer(parts):
                if not parts[1] or not parts[1].isdigit() or not (1 <= int(parts[1]) <= 120):
                    print(f"Record skipped [invalid age field]: {original_line}")
                elif not validate_phone(parts[3]):
                    print(f"Record skipped [invalid phone field]: {original_line}")
                continue
            customers[parts[0].lower()] = {
                'name': parts[0],
                'age': parts[1],
                'address': parts[2],
                'phone': parts[3]
            }
    print("Python DB server is now running...")
    return customers

def validate_customer(parts):
    if not parts[0] or not parts[0].isalpha():
        return False
    if parts[1] and (not parts[1].isdigit() or not (1 <= int(parts[1]) <= 120)):
        return False
    if parts[2] and not all(c.isalnum() or c in ' .-' for c in parts[2]):
        return False
    if parts[3] and not validate_phone(parts[3]):
        return False
    return True

def validate_phone(phone):
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

# SocketServer handler
class CustomerRequestHandler(socketserver.BaseRequestHandler):
    def handle(self):
        while True:
            self.data = self.request.recv(1024).strip()
            if not self.data:
                break
            command, *args = self.data.decode().split('|')
            response = getattr(self, f'handle_{command}', self.handle_unknown)(args)
            self.request.sendall(response.encode())

    def handle_find(self, args):
        name = args[0].lower()
        customer = self.server.customers.get(name)
        if customer:
            return f"{customer['name']}|{customer['age']}|{customer['address']}|{customer['phone']}"
        else:
            return f"{args[0]} not found in database"

    def handle_add(self, args):
        name, age, address, phone = args
        if name.lower() in self.server.customers:
            return "Customer already exists"
        if not validate_customer(args):
            return "Invalid customer data"
        self.server.customers[name.lower()] = {
            'name': name, 'age': age, 'address': address, 'phone': phone
        }
        return f"Customer {name} added successfully"

    def handle_delete(self, args):
        name = args[0].lower()
        if name in self.server.customers:
            del self.server.customers[name]
            return f"Customer {args[0]} deleted successfully"
        else:
            return f"Customer {args[0]} does not exist"

    def handle_update(self, args):
        name, field, value = args
        customer = self.server.customers.get(name.lower())
        if not customer:
            return f"Customer {name} not found"
        if field == 'age' and (not value.isdigit() or not (1 <= int(value) <= 120)):
            return "Invalid age"
        if field == 'address' and not all(c.isalnum() or c in ' .-' for c in value):
            return "Invalid address"
        if field == 'phone' and not validate_phone(value):
            return "Invalid phone number"
        customer[field] = value
        return f"Customer {name}'s {field} updated to {value}"

    def handle_report(self, args):
        # Sorting the customers by name before generating the report
        customers = sorted(self.server.customers.values(), key=lambda x: x['name'].lower())
        report = "\n".join(f"{c['name']}|{c['age']}|{c['address']}|{c['phone']}" for c in customers)
        return f"** Database contents **\n{report}"

    def handle_unknown(self, args):
        return "Unknown command"

class CustomerServer(socketserver.ThreadingTCPServer):
    def __init__(self, server_address, RequestHandlerClass, bind_and_activate=True):
        super().__init__(server_address, RequestHandlerClass, bind_and_activate)
        self.customers = load_database('data.txt')

if __name__ == '__main__':
    HOST, PORT = 'localhost', 9999
    with CustomerServer((HOST, PORT), CustomerRequestHandler) as server:
        server.serve_forever()
