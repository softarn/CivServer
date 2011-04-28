
#ifndef RESULT_H
#define RESULT_H


class Result
{
public:
	Result();
	virtual ~Result();

	virtual const char *getType() const = 0;
};

#endif // RESULT_H
