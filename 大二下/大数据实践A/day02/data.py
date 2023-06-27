# -*- coding:UTF-8 -*-
import random
import string
import sys
import time

# 大小写字母
alphabet_upper_list = string.ascii_uppercase
alphabet_lower_list = string.ascii_lowercase


# 随机生成指定位数的字符串
def get_random(instr, length):
    # 从指定序列中随机获取指定长度的片段并组成数组，例如:['a', 't', 'f', 'v', 'y']
    res = random.sample(instr, length)
    # 将数组内的元素组成字符串
    result = ''.join(res)
    return result

# 放置生成的并且不存在的rowkey
rowkey_tmp_list = []
# 制作rowkey
def get_random_rowkey():
    import time
    pre_rowkey = ""
    while True:
        # 获取00~99的两位数字，包含00与99
        num = random.randint(00, 99)
        # 获取当前10位的时间戳
        timestamp = int(time.time())
        # str(num).zfill(2)为字符串不满足2位，自动将该字符串补0
        pre_rowkey = str(num).zfill(2) + str(timestamp)
        if pre_rowkey not in rowkey_tmp_list:
            rowkey_tmp_list.append(pre_rowkey)
            break
    return pre_rowkey

# 创建用户名
def get_random_name(length):
    name = string.capwords(get_random(alphabet_lower_list, length))
    return name


# 获取年龄
def get_random_age():
    return str(random.randint(18, 60))


# 获取性别
def get_random_sex():
    return random.choice(["woman", "man"])


# 获取商品ID
def get_random_goods_no():
    goods_no_list = ["220902", "430031", "550012", "650012", "532120","230121","250983", "480071", "580016", "950013", "152121","230121"]
    return random.choice(goods_no_list)

# 获取商品价格（浮点型）
def get_random_goods_price():
	# 随机生成商品价格的整数位，1~999的三位数字，包含1与999
	price_int = random.randint(1, 999)
	# 随机生成商品价格的小数位，1~99的两位数字，包含1与99
	price_decimal = random.randint(1, 99)
	goods_price = str(price_int) +"." + str(price_decimal)
	return goods_price

# 获取门店ID
def get_random_store_id():
    store_id_list = ["313012", "313013", "313014", "313015", "313016","313017","313018", "313019", "313020", "313021", "313022","313023"]
    return random.choice(store_id_list)

# 获取购物行为类型
def get_random_goods_type():
    goods_type_list = ["pv", "buy", "cart", "fav","scan"]#点击、购买、加购、收藏、浏览
    return random.choice(goods_type_list)

# 获取电话号码
def get_random_tel():
    pre_list = ["130", "131", "132", "133", "134", "135", "136", "137", "138", "139", "147", "150",
                "151", "152", "153", "155", "156", "157", "158", "159", "186", "187", "188"]
    return random.choice(pre_list) + ''.join(random.sample('0123456789', 8))


# 获取邮箱名
def get_random_email(length):
    alphabet_list = alphabet_lower_list + alphabet_upper_list
    email_list = ["163.com", "126.com", "qq.com", "gmail.com","huawei.com"]
    return get_random(alphabet_list, length) + "@" + random.choice(email_list)


# 获取商品购买日期（统计最近7天数据）
def get_random_buy_time():
    buy_time_list = ["2019-08-01", "2019-08-02", "2019-08-03", "2019-08-04", "2019-08-05", "2019-08-06", "2019-08-07"]
    return random.choice(buy_time_list)

# 生成一条数据
def get_random_record():
    return get_random_rowkey() + "," + get_random_name(
        5) + "," + get_random_age() + "," + get_random_sex() + "," + get_random_goods_no() + ","+get_random_goods_price()+ "," +get_random_store_id()+ "," +get_random_goods_type() + ","+ get_random_tel() + "," + get_random_email(
        10) + "," +get_random_buy_time()

# 获取随机整数用于休眠
def get_random_sleep_time():
    return random.randint(5, 10)

# 将记录写到文本中
def write_record_to_file():
    # 覆盖文件内容，重新写入
    f = open(sys.argv[1], 'w')
    i = 0
    while i < int(sys.argv[2]):
        record = get_random_record()
        f.write(record)
        # 换行写入
        f.write('\n')
        i += 1
    f.close()

if __name__ == "__main__":
    write_record_to_file()



