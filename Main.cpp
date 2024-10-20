#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
#include <stdexcept>
#include <cstdint>

struct BigInt
{
    // Internal representation: least significant digit first
    std::vector<uint32_t> digits; // base 2^32
    bool negative;

    // Constructors
    BigInt() : negative(false) {}

    BigInt(const std::string &s)
    {
        negative = false;
        std::string str = s;
        if (str[0] == '-')
        {
            negative = true;
            str = str.substr(1);
        }

        // Remove leading zeros
        while (str.size() > 1 && str[0] == '0')
            str.erase(0, 1);

        digits.clear();
        BigInt result(0);
        for (char c : str)
        {
            result = result * 10 + BigInt(c - '0');
        }
        digits = result.digits;
    }

    BigInt(int value)
    {
        if (value < 0)
        {
            negative = true;
            value = -value;
        }
        else
        {
            negative = false;
        }
        if (value != 0)
            digits.push_back(static_cast<uint32_t>(value));
    }

    BigInt(uint32_t value) : negative(false)
    {
        if (value != 0)
            digits.push_back(value);
    }

    BigInt(int64_t value)
    {
        if (value < 0)
        {
            negative = true;
            value = -value;
        }
        else
        {
            negative = false;
        }
        if (value != 0)
        {
            digits.push_back(static_cast<uint32_t>(value & 0xFFFFFFFF));
            if (value > 0xFFFFFFFF)
                digits.push_back(static_cast<uint32_t>((value >> 32) & 0xFFFFFFFF));
        }
    }

    // Helper functions
    void trim()
    {
        while (digits.size() > 1 && digits.back() == 0)
            digits.pop_back();
        if (digits.size() == 1 && digits[0] == 0)
            negative = false;
    }

    bool isZero() const
    {
        return digits.empty() || (digits.size() == 1 && digits[0] == 0);
    }

    BigInt abs() const
    {
        BigInt result = *this;
        result.negative = false;
        return result;
    }

    // Addition
    BigInt operator+(const BigInt &other) const
    {
        if (negative == other.negative)
        {
            BigInt result;
            result.negative = negative;
            uint64_t carry = 0;
            size_t n = std::max(digits.size(), other.digits.size());
            for (size_t i = 0; i < n || carry; ++i)
            {
                uint64_t sum = carry;
                if (i < digits.size())
                    sum += digits[i];
                if (i < other.digits.size())
                    sum += other.digits[i];
                result.digits.push_back(static_cast<uint32_t>(sum & 0xFFFFFFFF));
                carry = sum >> 32;
            }
            return result;
        }
        else
        {
            return *this - (-other);
        }
    }

    // Unary minus
    BigInt operator-() const
    {
        BigInt result = *this;
        if (!isZero())
            result.negative = !negative;
        return result;
    }

    // Subtraction
    BigInt operator-(const BigInt &other) const
    {
        if (negative != other.negative)
        {
            return *this + (-other);
        }
        else
        {
            if (abs() >= other.abs())
            {
                BigInt result;
                result.negative = negative;
                int64_t borrow = 0;
                for (size_t i = 0; i < digits.size(); ++i)
                {
                    int64_t diff = static_cast<int64_t>(digits[i]) - borrow;
                    if (i < other.digits.size())
                        diff -= other.digits[i];
                    if (diff < 0)
                    {
                        diff += (1ULL << 32);
                        borrow = 1;
                    }
                    else
                    {
                        borrow = 0;
                    }
                    result.digits.push_back(static_cast<uint32_t>(diff & 0xFFFFFFFF));
                }
                result.trim();
                return result;
            }
            else
            {
                return -(other - *this);
            }
        }
    }

    // Multiplication
    BigInt operator*(const BigInt &other) const
    {
        BigInt result;
        result.digits.resize(digits.size() + other.digits.size());
        result.negative = negative != other.negative;
        for (size_t i = 0; i < digits.size(); ++i)
        {
            uint64_t carry = 0;
            for (size_t j = 0; j < other.digits.size() || carry; ++j)
            {
                uint64_t sum = result.digits[i + j] +
                               digits[i] * 1ULL * (j < other.digits.size() ? other.digits[j] : 0) + carry;
                result.digits[i + j] = static_cast<uint32_t>(sum & 0xFFFFFFFF);
                carry = sum >> 32;
            }
        }
        result.trim();
        return result;
    }

    // Division and Modulo
    BigInt operator/(const BigInt &other) const
    {
        BigInt quotient, remainder;
        divmod(*this, other, quotient, remainder);
        return quotient;
    }

    BigInt operator%(const BigInt &other) const
    {
        BigInt quotient, remainder;
        divmod(*this, other, quotient, remainder);
        return remainder;
    }

    // Bitwise AND
    BigInt operator&(const BigInt &other) const
    {
        BigInt result;
        size_t n = std::min(digits.size(), other.digits.size());
        for (size_t i = 0; i < n; ++i)
        {
            result.digits.push_back(digits[i] & other.digits[i]);
        }
        result.trim();
        return result;
    }

    // Bitwise OR
    BigInt operator|(const BigInt &other) const
    {
        BigInt result;
        size_t n = std::max(digits.size(), other.digits.size());
        for (size_t i = 0; i < n; ++i)
        {
            uint32_t a = i < digits.size() ? digits[i] : 0;
            uint32_t b = i < other.digits.size() ? other.digits[i] : 0;
            result.digits.push_back(a | b);
        }
        result.trim();
        return result;
    }

    // Input and Output
    friend std::istream &operator>>(std::istream &is, BigInt &bigint)
    {
        std::string s;
        is >> s;
        bigint = BigInt(s);
        return is;
    }

    friend std::ostream &operator<<(std::ostream &os, const BigInt &bigint)
    {
        if (bigint.isZero())
        {
            os << '0';
            return os;
        }
        if (bigint.negative)
            os << '-';

        // Convert digits to base 10
        BigInt temp = bigint.abs();
        std::string s;
        while (!temp.isZero())
        {
            uint32_t remainder = temp.divmod_small(10);
            s.push_back(static_cast<char>('0' + remainder));
        }
        std::reverse(s.begin(), s.end());
        os << s;
        return os;
    }

    // Shift operators
    BigInt operator<<(int shift) const
    {
        if (isZero() || shift == 0)
            return *this;
        BigInt result = *this;
        int word_shift = shift / 32;
        int bit_shift = shift % 32;
        uint32_t carry = 0;
        result.digits.insert(result.digits.begin(), word_shift, 0);
        for (size_t i = word_shift; i < result.digits.size(); ++i)
        {
            uint64_t current = (uint64_t(result.digits[i]) << bit_shift) | carry;
            result.digits[i] = static_cast<uint32_t>(current & 0xFFFFFFFF);
            carry = current >> 32;
        }
        if (carry)
            result.digits.push_back(static_cast<uint32_t>(carry));
        result.trim();
        return result;
    }

    BigInt operator>>(int shift) const
    {
        if (isZero() || shift == 0)
            return *this;
        BigInt result = *this;
        int word_shift = shift / 32;
        int bit_shift = shift % 32;
        if (word_shift >= static_cast<int>(result.digits.size()))
        {
            return BigInt(0);
        }
        result.digits.erase(result.digits.begin(), result.digits.begin() + word_shift);
        uint32_t carry = 0;
        for (int i = static_cast<int>(result.digits.size()) - 1; i >= 0; --i)
        {
            uint64_t current = (uint64_t(carry) << 32) | result.digits[i];
            result.digits[i] = static_cast<uint32_t>((current >> bit_shift) & 0xFFFFFFFF);
            carry = static_cast<uint32_t>(current & ((1ULL << bit_shift) - 1));
        }
        result.trim();
        return result;
    }

    // Shift-assignment operators
    BigInt &operator<<=(int shift)
    {
        return *this = *this << shift;
    }

    BigInt &operator>>=(int shift)
    {
        return *this = *this >> shift;
    }

    // Comparison operators
    bool operator<(const BigInt &other) const
    {
        if (negative != other.negative)
            return negative;
        if (digits.size() != other.digits.size())
            return negative ? digits.size() > other.digits.size() : digits.size() < other.digits.size();
        for (int i = static_cast<int>(digits.size()) - 1; i >= 0; --i)
        {
            if (digits[i] != other.digits[i])
                return negative ? digits[i] > other.digits[i] : digits[i] < other.digits[i];
        }
        return false;
    }

    bool operator>=(const BigInt &other) const
    {
        return !(*this < other);
    }

    // Equality operator
    bool operator==(const BigInt &other) const
    {
        return negative == other.negative && digits == other.digits;
    }

private:
    // Division by small integer
    uint32_t divmod_small(uint32_t divisor)
    {
        uint64_t remainder = 0;
        for (int i = static_cast<int>(digits.size()) - 1; i >= 0; --i)
        {
            uint64_t current = (remainder << 32) + digits[i];
            digits[i] = static_cast<uint32_t>(current / divisor);
            remainder = current % divisor;
        }
        trim();
        return static_cast<uint32_t>(remainder);
    }

    // Division and Modulo
    static void divmod(const BigInt &a, const BigInt &b, BigInt &quotient, BigInt &remainder)
    {
        if (b.isZero())
        {
            throw std::runtime_error("Division by zero");
        }
        if (a.isZero())
        {
            quotient = BigInt(0);
            remainder = BigInt(0);
            return;
        }
        quotient = BigInt(0);
        remainder = a.abs();
        BigInt divisor = b.abs();
        if (remainder < divisor)
        {
            quotient = BigInt(0);
            remainder = a;
            return;
        }
        int norm = 32 - __builtin_clz(divisor.digits.back());
        if (norm != 0)
        {
            divisor <<= norm;
            remainder <<= norm;
        }

        size_t n = remainder.digits.size();
        size_t m = divisor.digits.size();

        quotient.digits.resize(n - m + 1);
        for (int i = static_cast<int>(n - m); i >= 0; --i)
        {
            uint64_t r_hi = (i + m < remainder.digits.size()) ? remainder.digits[i + m] : 0;
            uint64_t r_lo = remainder.digits[i + m - 1];

            uint64_t numerator = (r_hi << 32) + r_lo;
            uint64_t denominator = divisor.digits.back();
            uint64_t qguess = numerator / denominator;
            if (qguess > 0xFFFFFFFF)
                qguess = 0xFFFFFFFF;

            BigInt mult = divisor * static_cast<uint32_t>(qguess);
            mult <<= 32 * i;
            while (remainder < mult)
            {
                --qguess;
                mult = divisor * static_cast<uint32_t>(qguess);
                mult <<= 32 * i;
            }
            quotient.digits[i] = static_cast<uint32_t>(qguess);
            remainder = remainder - mult;
        }
        quotient.trim();
        if (norm != 0)
            remainder >>= norm;
        quotient.negative = a.negative != b.negative;
        remainder.negative = a.negative;
        quotient.trim();
        remainder.trim();
    }
};

int main()
{
    BigInt a, b;
    std::cin >> a >> b;
    std::cout << "a + b = " << a + b << '\n';
    std::cout << "a - b = " << a - b << '\n';
    std::cout << "a * b = " << a * b << '\n';
    if (!b.isZero())
    {
        std::cout << "a / b = " << a / b << '\n';
        std::cout << "a % b = " << a % b << '\n';
    }
    std::cout << "a & b = " << (a & b) << '\n';
    std::cout << "a | b = " << (a | b) << '\n';
    return 0;
}
